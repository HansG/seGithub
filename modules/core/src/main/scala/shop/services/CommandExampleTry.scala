package shop.services

import cats.effect._
import cats.effect.std.Console
import cats.implicits.{
  catsSyntaxApplicativeError,
  catsSyntaxApply,
  toFlatMapOps,
  toFoldableOps,
  toFunctorOps,
  toTraverseOps
}
import cats.{ Applicative, Monad }
import fs2.Stream
import munit.{ CatsEffectSuite, ScalaCheckEffectSuite }
import natchez.Trace.Implicits.noop
import skunk.codec.all._
import skunk.implicits.toStringOps
import skunk.{ Command, Query, Session, SqlState, Void }

import java.time.OffsetDateTime

//https://tpolecat.github.io/skunk/tutorial/Command.html
class CommandExampleTry extends CatsEffectSuite with ScalaCheckEffectSuite {
  // a data type
  case class Pet(name: String, age: Short)

  // a service interface
  trait PetService[F[_]] {
    def insert(pet: Pet): F[Unit]
    def insert(ps: List[Pet]): F[Unit]
    def tryInsertAll(pets: List[Pet]): F[Unit]
    def selectAll: F[List[Pet]]
  }

  // a companion with a constructor
  object PetService {

    // command to insert a pet
    private val insertOne: Command[Pet] =
      sql"INSERT INTO pets VALUES ($varchar, $int2)".command
        .gcontramap[Pet]

    // command to insert a specific list of pets
    private def insertMany(ps: List[Pet]): Command[ps.type] = {
      val enc = (varchar ~ int2).gcontramap[Pet].values.list(ps)
      sql"INSERT INTO pets VALUES $enc".command
    }

    // query to select all pets
    private val all: Query[Void, Pet] =
      sql"SELECT name, age FROM pets"
        .query(varchar ~ int2)
        .gmap[Pet]

    // construct a PetService, preparing our statement once on construction
    def fromSession[F[_]: Monad: MonadCancelThrow: Console](s: Session[F]): F[PetService[F]] =
      s.prepare(insertOne).map { pc =>
        new PetService[F] {

          // Attempt to insert all pets, in a single transaction, handling each in turn and rolling
          // back to a savepoint if a unique violation is encountered. Note that a bulk insert with an
          // ON CONFLICT clause would be much more efficient, this is just for demonstration.
          def tryInsertAll(pets: List[Pet]): F[Unit] =
            s.transaction.use { xa =>
              pets.traverse_ { p =>
                for {
                  _  <- Console[F].println(s"Trying to insert $p")
                  sp <- xa.savepoint
                  _ <- pc.execute(p).recoverWith {
                    case SqlState.UniqueViolation(ex) =>
                      Console[F]
                        .println(s"Unique violation: ${ex.constraintName.getOrElse("<unknown>")}, rolling back...") *>
                        xa.rollback(sp)
                  }
                } yield ()
              }
            }

          def insert(pet: Pet): F[Unit] = pc.execute(pet).void

          def insert(ps: List[Pet]): F[Unit] = s.prepare(insertMany(ps)).flatMap(_.execute(ps)).void

          def selectAll: F[List[Pet]] = s.execute(all)
        }
      }
    /*
      new PetService[F] {
        def insert(pet: Pet): F[Unit] = s.prepare(insertOne).flatMap(_.execute(pet)).void
        def insert(ps: List[Pet]): F[Unit] = s.prepare(insertMany(ps)).flatMap(_.execute(ps)).void
        def selectAll: F[List[Pet]] = s.execute(all)
      }
   */

  }

  // a source of sessions
  val session: Resource[IO, Session[IO]] =
    Session.single(
      host = "localhost",
      user = "jimmy",
      database = "world",
      password = Some("banana")
    )

  // a resource that creates and drops a temporary table
  def withPetsTable(s: Session[IO]): Resource[IO, Unit] = {
    val alloc = s.execute(sql"CREATE TEMP TABLE pets (name varchar unique, age int2)".command).void
    val free  = s.execute(sql"DROP TABLE pets".command).void
    Resource.make(alloc)(_ => free)
  }

  // our entry point
  //  def run(args: List[String]): IO[ExitCode] =    ExitCode.Success
  test("insert et select") {
    session.flatTap(withPetsTable).evalMap(PetService.fromSession(_)).use { s =>
      for {
        _  <- s.insert(bob)
        _  <- s.insert(beagles)
        ps <- s.selectAll
        _  <- ps.traverse(p => IO.println(p))
      } yield ()
    }
  }

  test("noop") {
    IO(println("noop"))
  }

  // We can monitor the changing transaction status by tapping into the provided `fs2.Signal`
  def withTransactionStatusLogger[A](ss: Session[IO]): Resource[IO, Unit] = {
    val alloc: IO[Fiber[IO, Throwable, Unit]] =
      ss.transactionStatus.discrete.changes
        .evalMap(s => IO.println(s"xa status: $s"))
        .compile
        .drain
        .start
    Resource.make(alloc)(_.cancel).void
  }

  // A resource that puts it all together.
  val resource: Resource[IO, PetService[IO]] =
    for {
      s  <- session
      _  <- withPetsTable(s)
      _  <- withTransactionStatusLogger(s)
      ps <- Resource.eval(PetService.fromSession(s))
    } yield ps

  // some sample data
  val bob     = Pet("Bob", 12)
  val beagles = List(Pet("John", 2), Pet("George", 3), Pet("Paul", 6), Pet("Ringo", 3))
  val pets = List(
    Pet("Alice", 3),
    Pet("Bob", 42),
    Pet("Bob", 21),
    Pet("Steve", 9)
  )

  test("tryinsert et select") {
    resource.use { ps =>
      for {
        _   <- ps.tryInsertAll(pets)
        all <- ps.selectAll
        _   <- all.traverse_(p => IO.println(p))
      } yield ExitCode.Success
    }
  }

  // a data model
  case class Country(name: String, code: String, population: Int)

  // A service interface.
  trait Service[F[_]] {
    def currentTimestamp: F[OffsetDateTime]
    def countriesByName(pat: String): Stream[F, Country]
  }

  // A companion with a constructor.
  object Service {

    private val timestamp: Query[Void, OffsetDateTime] =
      sql"select current_timestamp".query(timestamptz)

    private val countries: Query[String, Country] =
      sql"""
        SELECT name, code, population
        FROM   country
        WHERE  name like $text
      """
        .query(varchar ~ bpchar(3) ~ int4)
        .gmap[Country]

    def fromSession[F[_]: Applicative](s: Session[F]): F[Service[F]] =
      s.prepare(countries).map { pq =>
        // Our service implementation. Note that we are preparing the query on construction, so
        // our service can run it many times without paying the planning cost again.
        new Service[F] {
          def currentTimestamp: F[OffsetDateTime]              = s.unique(timestamp)
          def countriesByName(pat: String): Stream[F, Country] = pq.stream(pat, 32)
        }

      }
  }

  // A source of services
  val service: Resource[IO, Service[IO]] =
    session.evalMap(Service.fromSession(_))

  // our entry point ... there is no indication that we're using a database at all!
  test("service test") {
    service.use { s =>
      for {
        ts <- s.currentTimestamp
        _  <- IO.println(s"timestamp is $ts")
        _ <- s
          .countriesByName("U%")
          .evalMap(c => IO.println(c))
          .compile
          .drain
      } yield ()
    }
  }

}
