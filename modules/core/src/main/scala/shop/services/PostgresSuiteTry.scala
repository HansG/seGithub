package shop.services

import org.scalacheck.Gen
import shop.domain.brand.{Brand, BrandId, BrandName}
import cats.Monad
import cats.effect._
import cats.implicits.{catsSyntaxOptionId, none, toFlatMapOps, toFunctorOps, toTraverseOps}
import monocle.Iso
import munit.CatsEffectSuite
import skunk._
import skunk.implicits._
import skunk.codec.all._
import natchez.Trace.Implicits.noop
import shop.domain.ID
import shop.domain.auth.{EncryptedPassword, UserName}
import shop.effects.GenUUID
import shop.optics.IsUUID
import shop.services.PostgresSuiteTry.Res

import java.util.UUID

object PostgresSuiteTry {
  import StartPostgres._

  type Res = Resource[IO, Session[IO]]

  case class BrandIdT(value: UUID)
  object BrandIdT {
    implicit val isuuid: IsUUID[BrandIdT] = new IsUUID[BrandIdT] {
      val _UUID: Iso[UUID, BrandIdT] = Iso[UUID, BrandIdT](BrandIdT(_))(_.value)
    }
  }
  case class BrandNameT(value: String)

  case class BrandT(uuid: BrandIdT, name: BrandNameT)
//codecs
  val brandIdT: Codec[BrandIdT]     = uuid.imap[BrandIdT](BrandIdT(_))(_.value)
  val brandNameT: Codec[BrandNameT] = varchar.imap[BrandNameT](BrandNameT(_))(_.value)
  val brandT: Codec[BrandT] =
    (brandIdT ~ brandNameT).imap {
      case i ~ n => BrandT(i, n)
    }(b => b.uuid ~ b.name)

  val selectAll: Query[Void, BrandT] =
    sql"""
        SELECT * FROM brands
       """.query(brandT)

  val insertBrand: Command[BrandT] =
    sql"""
        INSERT INTO brands
        VALUES ($brandT)
        """.command

  trait BrandsT[F[_]] {
    def findAll: F[List[BrandT]]
    def create(name: BrandNameT): F[BrandIdT]
  }

  object BrandsT {
    def make[F[_]: GenUUID: MonadCancelThrow](
        postgres: Resource[F, Session[F]]
    ): BrandsT[F] =
      new BrandsT[F] {

        def findAll: F[List[BrandT]] =
          postgres.use(_.execute(selectAll))

        def create(name: BrandNameT): F[BrandIdT] =
          postgres.use { session =>
            session.prepare(insertBrand).flatMap { cmd =>
              ID.make[F, BrandIdT].flatMap { id =>
                cmd.execute(BrandT(id, name)).as(id)
              }
            }
          }
      }
  }

  def idGen[A](f: UUID => A): Gen[A] =
    Gen.uuid.map(f)

  lazy val brandIdTGen: Gen[BrandIdT] =
    idGen(BrandIdT.apply)

  lazy val nonEmptyStringGen: Gen[String] =
    Gen
      .chooseNum(7, 15)
      .flatMap { n =>
        Gen.buildableOfN[String, Char](n, Gen.alphaChar)
      }
      .map(_.toLowerCase.capitalize)

  def nesGen[A](f: String => A): Gen[A] =
    nonEmptyStringGen.map(f)

  lazy val brandNameTGen: Gen[BrandNameT] =
    nesGen(BrandNameT.apply)

  lazy val brandTGen: Gen[BrandT] =
    for {
      i <- brandIdTGen
      n <- brandNameTGen
    } yield BrandT(i, n)

  val brandTLGen: Gen[List[BrandT]] =
    Gen
      .chooseNum(1, 10)
      .flatMap { n =>
        Gen.buildableOfN[List[BrandT], BrandT](n, brandTGen)
      }

  val flushTables: Command[Void] = sql"DELETE FROM #brands".command

  val brandTSession        = BrandsT.make[IO](singleSession)
  val brand: BrandT        = brandTGen.sample.get
  val brandL: List[BrandT] = brandTLGen.sample.get
  def trys(brand: BrandT): IO[Unit] =
    for {
      x <- brandTSession.findAll
      _ <- brandTSession.create(brand.name)
      y <- brandTSession.findAll
      z <- brandTSession.create(brand.name).attempt
    } yield println(
      x.toString() + "\n" + (y.count(_.name == brand.name) == 1) + "\n" + (z
        .fold(_.printStackTrace(), "Neu: " + _.toString))
    )

  // res.unsafeRunSync()



  //gen zu stream + parMap mit pooledSessions

  val test0 = singleSession.use { s => // (3)
    for {
      d <- s.unique(sql"select current_date".query(date)) // (4)
      _ <- IO.println(s"The current date is $d.")
    } yield ExitCode.Success
  }

}



object StartPostgres extends App {
  // os.proc("""C:\se\prog\postgresql\12\bin\pg_ctl.exe""", "runservice",  """-N "postgresql-x64-12"""", """-D "C:\se\prog\postgresql\12\data"""",  "-w")

//    Sync[F].delay(new Configuration(system))
//      .flatMap(configure)
//      .flatTap(GlobalTracer.registerTracer[F])

  Resource.make( IO(os.proc("""C:\se\prog\postgresql\12\pgAdmin 4\bin\pgAdmin4.exe""").call() ) )(c => IO(println(c.toString()) ))
    .useForever
//    .map { new JaegerEntryPoint[F](_, uriPrefix) }



  lazy val pooledSessions: Resource[IO, Resource[IO, Session[IO]]] =
    Session.pooled[IO](
      host = "localhost",
      port = 5432,
      user = "postgres",
      password = Some("postgres"),
      database = "store",
      max = 10
    )

  lazy val singleSession: Res =
    Session.single[IO](
      host = "localhost",
      port = 5432,
      user = "postgres",
      password = Some("postgres"),
      database = "store"
    )

}


object UserTry {
  case class UserT(id: UUID, name: String, pwd: String)

  val codec = (uuid ~ varchar ~ varchar).imap {
    case id ~ name ~ pwd => UserT(id, name, pwd)
  } {
    case u => u.id ~ u.name ~ u.pwd
  }
  val insertSql: Command[((UUID, String), String)] = sql"insert into users VALUES ($uuid, $varchar, $varchar)".command
  val insertSqlC = sql"insert into users VALUES $codec".command

  def insert(dbres: Res, username: String, password: String) =
    dbres.use(s => insertS(s,username, password))
  def insertS(s: Session[IO], username: String, password: String) =
  //  s.prepare(insertSqlC).flatMap(pc => pc.execute(UserT(UUID.randomUUID(), username, password)))
    s.prepare(insertSql).flatMap(pc => pc.execute((UUID.randomUUID() ~ username ~ password)))

  val findSql: Query[String, UserT] = sql"select * from users where name = $varchar".query(codec)

  def findUser[A]( where : Fragment[A]): Query[A, UserT] = sql"select * from users $where".query(codec)
  val frag : Fragment[String] =  sql"name = $varchar"
  def findUserByNameQ: Query[String, UserT] = findUser(frag)
  val findAllUserQ: Query[Void, UserT] = findUser(Fragment.empty)


  def findS(s: Session[IO], uname: String): IO[Option[UserT]] =
    s.prepare(findUserByNameQ).flatMap { q =>
      q.option(uname).map {
        case Some(u) => u.some
        case _       => none[UserT]
      }
    }

  def findAllUserS(s : Session[IO]) =  s.prepare(findAllUserQ).flatMap { q =>
    q.stream(Void,32).evalTap(u => IO(println(u))).compile.toList
  }

  def findAllUserS(s : Session[IO], uname: String) =  s.prepare(findUserByNameQ).flatMap { pq =>
    pq.stream(uname, 32).evalTap(u => IO(println(u))).compile.toList
  }


  def find(dbres: Res, uname: String) = dbres.use { s => findS(s, uname) }

  def insertEtFind(dbres: Res, username: String, password: String) =
    for {
      d <- insert(dbres, username, password)
      x <- find(dbres, username)
    } yield x.count(_.id == d)

 def insertEtFindS(s: Session[IO], username: String, password: String): IO[Int] =
    for {
      d <- insertS(s, username, password)
      x <- findAllUserS(s, username)
    } yield x.count(_.id == d)

}


class TestTry extends CatsEffectSuite {
  import UserTry._


  test("findUser") {
    StartPostgres.singleSession.use {  findAllUserS }
  }

  test("single Session") {
    StartPostgres.singleSession.use( s =>
      List(("Ha", "aH"), ("Sa", "aS"), ("Mo", "om"), ("Ad", "da")).traverse((vn) => UserTry.insertEtFindS(s, vn._1, vn._2).flatTap(n => IO.println(s"Gefnden mit id $n")) )
    )
    //    UserTry.test(singleSession, "Ha", "aH") .as(ExitCode.Success)
    //    UserTry.test(singleSession, "Sa", "aS") .as(ExitCode.Success)
    //    UserTry.test(singleSession, "Mo", "om") .as(ExitCode.Success)
    //    UserTry.test(singleSession, "Ad", "da") .as(ExitCode.Success)
  }

  test("single SessionRes") {
      List(("Ha", "aH"), ("Sa", "aS"), ("Mo", "om"), ("Ad", "da")).traverse((vn) => UserTry.insertEtFind(StartPostgres.singleSession, vn._1, vn._2))
    //    UserTry.test(singleSession, "Ha", "aH") .as(ExitCode.Success)
    //    UserTry.test(singleSession, "Sa", "aS") .as(ExitCode.Success)
    //    UserTry.test(singleSession, "Mo", "om") .as(ExitCode.Success)
    //    UserTry.test(singleSession, "Ad", "da") .as(ExitCode.Success)
  }

  test("SessionPool") {
    StartPostgres.pooledSessions.use { resS =>
      List(("Ha", "aH"), ("Sa", "aS"), ("Mo", "om"), ("Ad", "da")).traverse((vn) => UserTry.insertEtFind(resS, vn._1, vn._2))
    }
  }


  /*
  https://stackoverflow.com/questions/60438969/postgresql-npgsql-returning-42601-syntax-error-at-or-near-1
  using (Npgsql.NpgsqlConnection conn = new Npgsql.NpgsqlConnection(DBManager.GetConnectionString()))
          {
              conn.Open();
              Logger.Info("connection opened for adding column");
              using (Npgsql.NpgsqlCommand addColumnQuery = new Npgsql.NpgsqlCommand(@"ALTER TABLE @tableName ADD COLUMN IF NOT EXISTS @columnName  @columnType;", conn))
              {
                  addColumnQuery.Parameters.AddWithValue("@tableName", tableName);
                  addColumnQuery.Parameters.AddWithValue("@columnName", columnName);
                  addColumnQuery.Parameters.AddWithValue("@columnType", columnType);
                  addColumnQuery.ExecuteNonQuery();
              }
          }
   */


}






//https://tpolecat.github.io/skunk/tutorial/Command.html
object CommandExampleTry {
  // a data type
  case class Pet(name: String, age: Short)

  // a service interface
  trait PetService[F[_]] {
    def insert(pet: Pet): F[Unit]
    def insert(ps: List[Pet]): F[Unit]
    def selectAll: F[List[Pet]]
  }

  // a companion with a constructor
  object PetService {

    // command to insert a pet
    private val insertOne: Command[Pet] =
      sql"INSERT INTO pets VALUES ($varchar, $int2)"
        .command
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

    // construct a PetService
    def fromSession[F[_]: Monad](s: Session[F]): PetService[F] =
      new PetService[F] {
        def insert(pet: Pet): F[Unit] = s.prepare(insertOne).flatMap(_.execute(pet)).void
        def insert(ps: List[Pet]): F[Unit] = s.prepare(insertMany(ps)).flatMap(_.execute(ps)).void
        def selectAll: F[List[Pet]] = s.execute(all)
      }

  }


  // a source of sessions
  val session: Resource[IO, Session[IO]] =
    Session.single(
      host     = "localhost",
      user     = "jimmy",
      database = "world",
      password = Some("banana"),
    )

  // a resource that creates and drops a temporary table
  def withPetsTable(s: Session[IO]): Resource[IO, Unit] = {
    val alloc = s.execute(sql"CREATE TEMP TABLE pets (name varchar, age int2)".command).void
    val free  = s.execute(sql"DROP TABLE pets".command).void
    Resource.make(alloc)(_ => free)
  }

  // some sample data
  val bob     = Pet("Bob", 12)
  val beagles = List(Pet("John", 2), Pet("George", 3), Pet("Paul", 6), Pet("Ringo", 3))

  // our entry point
  def run(args: List[String]): IO[ExitCode] =
    session.flatTap(withPetsTable).map(PetService.fromSession(_)).use { s =>
      for {
        _  <- s.insert(bob)
        _  <- s.insert(beagles)
        ps <- s.selectAll
        _  <- ps.traverse(p => IO.println(p))
      } yield ExitCode.Success
    }





}
