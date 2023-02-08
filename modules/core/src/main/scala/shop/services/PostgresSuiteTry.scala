package shop.services

import org.scalacheck.Gen
import shop.domain.brand.{ Brand, BrandId, BrandName }
import cats.effect._
import cats.implicits.{ catsSyntaxOptionId, none, toFlatMapOps, toFunctorOps }
import monocle.Iso
import skunk._
import skunk.implicits._
import skunk.codec.all._
import natchez.Trace.Implicits.noop
import shop.domain.ID
import shop.domain.auth.{ EncryptedPassword, UserName }
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

  object UserTry {
    case class UserT(id: UUID, name: String, pwd: String)

    val codec = (uuid ~ varchar ~ varchar).imap {
      case id ~ name ~ pwd => UserT(id, name, pwd)
    } {
      case u => u.id ~ u.name ~ u.pwd
    }
    val insertSql = sql"insert into users $codec".command

    def insert(dbres: Res, username: String, password: String) =
      dbres.use(s => s.prepare(insertSql).flatMap(pc => pc.execute(UserT(UUID.randomUUID(), username, password))))

    val findSql = sql"select * from users where username = $varchar".query(codec)

    def find(dbres: Res, uname: String) = dbres.use { s =>
      s.prepare(findSql).flatMap { q =>
        q.option(uname).map {
          case Some(u) => u.some
          case _       => none[UserT]
        }
      }
    }

    def test(dbres: Res, username: String, password: String) =
      for {
        d <- insert(dbres, username, password)
        x <- find(dbres, username)
      } yield x.count(_.id == d)

  }





  //gen zu stream + parMap mit pooledSessions

  val test0 = singleSession.use { s => // (3)
    for {
      d <- s.unique(sql"select current_date".query(date)) // (4)
      _ <- IO.println(s"The current date is $d.")
    } yield ExitCode.Success
  }

}

object PostgresTestTry extends IOApp {
  import PostgresSuiteTry._
  import StartPostgres._
  //def run(args: List[String]): IO[ExitCode] = trys(brand).as(ExitCode.Success)
  def run(args: List[String]): IO[ExitCode] = {
    UserTry.test(singleSession, "Ha", "aH") .as(ExitCode.Success)
    UserTry.test(singleSession, "Sa", "aS") .as(ExitCode.Success)
    UserTry.test(singleSession, "Mo", "om") .as(ExitCode.Success)
    UserTry.test(singleSession, "Ad", "da") .as(ExitCode.Success)
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
