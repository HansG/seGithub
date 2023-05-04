// Copyright (c) 2018-2021 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package shop.domain.mongotry

import cats._
import cats.effect._
import cats.effect.std.Console
import cats.syntax.all._
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import fs2.Stream
import fs2.io.net.Network
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._
import munit.CatsEffectSuite
import natchez.Trace
import natchez.Trace.Implicits.noop
import org.http4s.HttpRoutes
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import shop.services.Http4sExample
import skunk.codec.text.{bpchar, varchar}
import org.http4s.circe.CirceEntityEncoder._
import skunk.implicits._
import skunk.{Fragment, Query, Session, Void}
import mongo4cats.bson.ObjectId
import mongo4cats.client.MongoClient
import mongo4cats.circe.{instantDecoder => _, instantEncoder => _, _}
import mongo4cats.database.GenericMongoDatabase
import mongo4cats.operations.Filter
import munit.CatsEffectSuite
import mongo4cats.circe._
import org.bson.BsonValue
import org.http4s.server.Server
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.math.BigInteger
import scala.jdk.CollectionConverters._
import java.util.UUID

//object Http4sMongoTry extends CatsEffectSuite {
object Http4sMongoTry extends IOApp {

  /** A data model with a Circe `Encoder` */
  @derive(encoder, decoder)
  case class Country(code: String, name: String)

  /* object Country {
    implicit val encoderCountry: Encoder[Country] = deriveEncoder
  }*/

  /** A service interface and companion factory method. */
  trait CountryService[F[_]] {
    def byCode(code: String): F[Option[Country]]
    def all: F[Stream[F, Country]]

    def save(cl: List[Country]): F[List[BigInteger]]

  }

  /** Given a `Session` we can create a `Countries` resource with pre-prepared statements. */
  def countryServicePFrom[F[_]: Monad: MonadCancel[*[_], Throwable]](
      resSession: Resource[F, Session[F]]
  ): CountryService[F] = {

    def countryQuery[A](where: Fragment[A]): Query[A, Country] =
      sql"SELECT code, name FROM country $where".query((bpchar(3) ~ varchar).gmap[Country])

    new CountryService[F] {
      def byCode(code: String): F[Option[Country]] =
        resSession.use { sess =>
          sess.prepare(countryQuery(sql"WHERE code = ${bpchar(3)}")).flatMap { psByCode =>
            psByCode.option(code)
          }
        }

      def all: F[Stream[F, Country]] =
        resSession.use { sess =>
          sess.prepare(countryQuery(Fragment.empty)).map { psAll =>
            psAll.stream(Void, 64)
          }
        }

      def save(cl: List[Country]): F[List[BigInteger]] = Monad[F].unit.as(List())

    }
  }

  def countryServiceFrom[F[_]: Monad: MonadCancel[*[_], Throwable]](
      mongoClient: MongoClient[F], tablename : String = "country"
  ): CountryService[F] = {

    val countryColl = mongoClient.getDatabase("testdb").flatMap { db =>
      db.getCollectionWithCodec[Country](tablename)
    }

    new CountryService[F] {
      def byCode(code: String): F[Option[Country]] =
        countryColl.flatMap { coll =>
          coll.find.filter(Filter.eq("code", code)).first
        }

      def all =
        countryColl.map { coll =>
          coll.find.stream
        }

      override def save(cl: List[Country]): F[List[BigInteger]] = countryColl.flatMap { coll =>
        coll
          .insertMany(cl)
          .map(im => {
            im.getInsertedIds.asScala.values.map { v =>
              new BigInteger(v.asObjectId.getValue.toByteArray)
            }.toList
          })

      }
    }
  }

  /*
  test("PersonG mongo") {
    mongoClientRes.use { client =>
      for {
        db <- client.getDatabase("testdb")
        //coll <- db.createCollection("country")
        coll      <- db.getCollectionWithCodec[Country]("country")
        someCount <- coll.find.filter(Filter.eq("code", code)).all
        _         <- IO.println(somePers)
        allPers   <- coll.find.stream.compile.toList
        _         <- IO.println(allPers)
      } yield ()
    }
  }
   */
  /** Resource yielding a pool of `CountryService`, backed by a single `Blocker` and `SocketGroup`. */
  def countryServiceFromConnectionString[F[_]: Async](connectionString: String, tablename : String = "country"): Resource[F, CountryService[F]] =
    MongoClient.fromConnectionString[F](connectionString).map(countryServiceFrom(_, tablename))

  def countryServiceP[F[_]: Async: Console]: Resource[F, CountryService[F]] =
    Session
      .pooled[F](
        host = "localhost",
        port = 5432,
        user = "jimmy",
        password = Some("banana"),
        database = "world",
        max = 10,
        commandCache = 0,
        queryCache = 0
      )
      .map(countryServicePFrom(_))

  /** Given a pool of `Countries` we can create an `HttpRoutes`. */
  def routesFrom[F[_]: Concurrent](
      countryService: CountryService[F]
  ): HttpRoutes[F] = {
    object dsl extends Http4sDsl[F];
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "country1" / code =>
        Ok(
          countryService
            .byCode(code)
            .map(oc => oc.fold(s"No country found with code $code.")((c: Country) => c.asJson.spaces4))
        )

      case GET -> Root / "country" / code =>
        countryService.byCode(code).flatMap {
          case Some(c) => Ok(c.asJson)
          case None    => NotFound(s"No country has code $code.")
        }

      case GET -> Root / "countries" =>
        countryService.all.flatMap { st =>
          val stt = st.compile.toList.map(_.asJson) //how to use stream directly in the response?
          Ok(stt)
        }
    }
  }

  /** Our application as a resource. */
  def resServer[F[_]: Async: Console]: Resource[F, Server] =
    countryServiceFromConnectionString[F]("mongodb://localhost:27017")
      .map {
        //    countryServiceP.map {
        countryService =>
          val routes = routesFrom(countryService)
          Http4sExample.httpAppFrom(routes)
      }
      .flatMap { app =>
        Http4sExample.resServer(app)
      }

  def transferData[F[_]: Concurrent: Async: Console: Trace](csQuelle: CountryService[F], csZiel: CountryService[F]): F[Unit] = {
    for {
      st <- csQuelle.all
      cst = st.chunkN(10, true)
      u <- cst
        .evalTap(chunk =>  Console[F].println(s"Current chunk: $chunk")  )
        .evalMap(chunk =>   csZiel.save(chunk.toList) )
        .evalTap(ids =>  Console[F].println(s"Current Ids: $ids")  )
        .compile.drain
    } yield u
  }

  implicit val logger = Slf4jLogger.getLogger[IO]

  /** Main method instantiates `F` to `IO` and `use`s our resource forever. */
  def run(args: List[String]): IO[ExitCode] =
    run2(args)

  def run1(args: List[String]): IO[ExitCode] =
    resServer[IO].useForever

  case class ServicePair[F[_]](s1 : Resource[F, CountryService[F]], s2 : Resource[F, CountryService[F]])
 def run2(args: List[String]): IO[ExitCode] = {
   (countryServiceP[IO] , countryServiceFromConnectionString[IO]("mongodb://localhost:27017", "countryT")).mapN(ServicePair[IO](_,_)).use { sp =>
     transferData(sp.s1,sp.s1)
   }.as(ExitCode.Success)
 }

}
