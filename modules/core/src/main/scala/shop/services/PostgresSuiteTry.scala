package shop.services

import org.scalacheck.Gen
import shop.domain.brand.{Brand, BrandId, BrandName}
import cats.effect._
import monocle.Iso
import skunk._
import skunk.implicits._
import skunk.codec.all._
import natchez.Trace.Implicits.noop
import shop.domain.ID
import shop.effects.GenUUID
import shop.optics.IsUUID

import java.util.UUID

object PostgresSuiteTry extends IOApp {

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
  val codecT: Codec[BrandT] =
    (brandIdT ~ brandNameT).imap {
      case i ~ n => BrandT(i, n)
    }(b => b.uuid ~ b.name)

  val selectAll: Query[Void, BrandT] =
    sql"""
        SELECT * FROM brands
       """.query(codecT)

  val insertBrand: Command[BrandT] =
    sql"""
        INSERT INTO brands
        VALUES ($codecT)
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
            session.prepare(insertBrand).use { cmd =>
              ID.make[F, BrandIdT].flatMap { id =>
                cmd.execute(Brand(id, name)).as(id)
              }
            }
          }
      }
  }










  def idGen[A](f: UUID => A): Gen[A] =
    Gen.uuid.map(f)

  lazy val brandIdGen: Gen[BrandIdT] =
    idGen(BrandIdT.apply)

  lazy val nonEmptyStringGen: Gen[String] =
    Gen
      .chooseNum(7, 15)
      .flatMap { n =>
        Gen.buildableOfN[String, Char](n, Gen.alphaChar)
      }.map(_.toLowerCase.capitalize)

  def nesGen[A](f: String => A): Gen[A] =
    nonEmptyStringGen.map(f)

  lazy val brandNameGen: Gen[BrandNameT] =
    nesGen(BrandNameT.apply)

  lazy val brandGen: Gen[BrandT] =
    for {
      i <- brandIdGen
      n <- brandNameGen
    } yield BrandT(i, n)


  val flushTables: Command[Void] = sql"DELETE FROM #brands".command


  val singleSession: Res =
    Session.single[IO](
      host = "localhost",
      port = 5432,
      user = "postgres",
      password = Some("postgres"),
      database = "store"
    )

  val brandSession     = Brands.make[IO](singleSession)
  val brand = brandGen.sample.get
  val res = for {
    x <- brandSession.findAll
    _ <- brandSession.create(brand.name)
    y <- brandSession.findAll
    z <- brandSession.create(brand.name).attempt
  } yield println( x.toString() +"\n"+  (y.count(_.name == brand.name) == 1) + "\n" + z.isLeft )

 // res.unsafeRunSync()


  def run(args: List[String]): IO[ExitCode] =  res.as(ExitCode.Success)


  val test0 = singleSession.use { s => // (3)
      for {
        d <- s.unique(sql"select current_date".query(date)) // (4)
        _ <- IO.println(s"The current date is $d.")
      } yield ExitCode.Success
    }

}
