package shop.services

import org.scalacheck.Gen
import shop.domain.brand.{Brand, BrandId, BrandName}
import cats.effect._
import skunk._
import skunk.implicits._
import skunk.codec.all._
import natchez.Trace.Implicits.noop
import java.util.UUID

object PostgresSuiteTry extends IOApp {

  type Res = Resource[IO, Session[IO]]

  def idGen[A](f: UUID => A): Gen[A] =
    Gen.uuid.map(f)

  lazy val brandIdGen: Gen[BrandId] =
    idGen(BrandId.apply)

  def nesGen[A](f: String => A): Gen[A] =
    Gen.alphaStr.map(f)

  lazy val brandNameGen: Gen[BrandName] =
    nesGen(BrandName.apply)

  lazy val brandGen: Gen[Brand] =
    for {
      i <- brandIdGen
      n <- brandNameGen
    } yield Brand(i, n)


  val flushTables: Command[Void] = sql"DELETE FROM #brands".command


  val singleConn: Res =
    Session.single[IO](
      host = "localhost",
      port = 5432,
      user = "postgres",
      password = Some("my-password"),
      database = "store"
    )

  val b     = Brands.make[IO](singleConn)
  val brand = brandGen.sample.get
  val res = for {
    x <- b.findAll
    _ <- b.create(brand.name)
    y <- b.findAll
    z <- b.create(brand.name).attempt
  } yield println( x.toString() +"\n"+  (y.count(_.name == brand.name) == 1) + "\n" + z.isLeft )

 // res.unsafeRunSync()


  def run(args: List[String]): IO[ExitCode] =  res.as(ExitCode.Success)


  val test0 = singleConn.use { s => // (3)
      for {
        d <- s.unique(sql"select current_date".query(date)) // (4)
        _ <- IO.println(s"The current date is $d.")
      } yield ExitCode.Success
    }

}
