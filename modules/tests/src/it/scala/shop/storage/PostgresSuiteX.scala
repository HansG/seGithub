package shop.storage

import cats.data.NonEmptyList
import cats.effect._
import cats.effect.unsafe.implicits.global
import cats.implicits._
import natchez.Trace.Implicits.noop
import org.scalacheck.Gen
import shop.domain._
import shop.domain.brand._
import shop.domain.category._
import shop.domain.item._
import shop.generators._
import shop.services._
import skunk._
import skunk.implicits._
import suite.ResourceSuite

object PostgresSuiteX extends App {

  type Res = Resource[IO, Session[IO]]

  val singleConn: Res =
    Session.single(
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

  res.unsafeRunSync()
}
