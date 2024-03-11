package shop.services

import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import cats.{Applicative, Monad}
import cats.effect._
import cats.effect.std.Console
import fs2.Stream
import StartPostgres._
import brandDomain._
import brandAlg._
import brandTestGen._
import cats.implicits.{toFlatMapOps, toTraverseOps}
import mongo4cats.client.MongoClient
import org.scalacheck.effect.PropF
import shop.services.brandPG.selectAll

import java.util.UUID

class DatabaseTestTry extends CatsEffectSuite with ScalaCheckEffectSuite {

  def findCreate2PG(res: Res, brand: BrandT): IO[Unit] = {
    val brandsRes = res.flatTap(withTempTable).map(BrandsT.makePg(_))
    findCreate2ByAlg(brandsRes, brand)
  }

  def findCreate2MG(res: Resource[IO, MongoClient[IO]], brand: BrandT): IO[Unit] = {
    val brandsRes = res.map(BrandsT.makeMG(_))
    findCreate2ByAlg(brandsRes, brand)
  }

  def findCreate2ByAlg(res: Resource[IO, BrandsT[IO]], brand: BrandT): IO[Unit] =
    res.use {
      findCreateUsingService(_, brand)
    }

  def findCreateUsingService(bs : BrandsT[IO], brand: BrandT) = {
    for {
      x <- bs.findAll
      _ <- bs.create(brand.name)
      y <- bs.findAll
      z <- bs.create(brand.name).attempt
    } yield println(
      x.toString() + "\n" + (y.count(_.name == brand.name) == 1) + "\n" + (z
        .fold(_.printStackTrace(), "Neu: " + _.toString))
    )
  }

  test("single brand") {
    findCreate2PG(singleSession, BrandT(brandIdGen.sample.get,  BrandNameT("PPSampleXXX")))
  }

  test("list brand simple") {
    singleSession.use(s => s.execute(selectAll).flatTap(li => IO(println(li))))
  }

  test("single brand MG") {
    val brand = BrandT(BrandIdT(new UUID(54322L, 12345L)), BrandNameT("Daserwx"))
    findCreate2MG(mongoClientRes, brandSample)
  }


  test("list brand") {
    brandSampleList.traverse(br => findCreate2PG(singleSession, br))
  }

  test("list brand using Service") {
    val brandsRes = singleSession.map(BrandsT.makePg(_))//.flatTap(withTempTable)
    brandsRes.use( bs =>  brandSampleList.traverse(br => findCreateUsingService(bs, br)) )
  }

  test("list brand") {
    PropF.forAllF(brandGen) { brand =>
      //   forAll(brandTGen) { brand =>
      findCreate2PG(singleSession, brand).as(())
    }
  }


}
