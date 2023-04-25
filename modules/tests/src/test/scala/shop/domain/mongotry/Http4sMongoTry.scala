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
import org.http4s.server.Server

object Http4sMongoTry extends CatsEffectSuite {

  /** A data model with a Circe `Encoder` */
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
  }

  /** Given a `Session` we can create a `Countries` resource with pre-prepared statements. */
  def resCountryServiceP[F[_]: Monad: MonadCancel[*[_], Throwable]](
      resSession: Resource[F, Session[F]]
  ): Resource[F, CountryService[F]] = {

    def countryQuery[A](where: Fragment[A]): Query[A, Country] =
      sql"SELECT code, name FROM country $where".query((bpchar(3) ~ varchar).gmap[Country])

    resSession.map { sess =>
      new CountryService[F] {
        def byCode(code: String): F[Option[Country]] =
          sess.prepare(countryQuery(sql"WHERE code = ${bpchar(3)}")).flatMap { psByCode =>
            psByCode.option(code)
          }

        def all: F[Stream[F, Country]] =
          sess.prepare(countryQuery(Fragment.empty)).map { psAll =>
            psAll.stream(Void, 64)
          }
      }
    }
  }

  def resCountryService[F[_]: Monad: MonadCancel[*[_], Throwable]](
      mongoClientRes: Resource[F, MongoClient[F]]
  ): Resource[F, CountryService[F]] = {

    def mongoCollRes = mongoClientRes.evalMap { client =>
      for {
        db <- client.getDatabase("testdb")
        //coll <- db.createCollection("country")
        coll <- db.getCollectionWithCodec[Country]("country")
      } yield coll
    }

    mongoCollRes.map { coll =>
        new CountryService[F] {
          def byCode(code: String): F[Option[Country]] =
            coll.find.filter(Filter.eq("code", code)).first

          def all: Stream[F, Country] =  coll.find.stream
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
  def resResCountryService[F[_]: Async] : Resource[F,  CountryService[F]] =
    resCountryService(MongoClient.fromConnectionString[F]("mongodb://localhost:27017") )

  /** Given a pool of `Countries` we can create an `HttpRoutes`. */
  def resRoutes[F[_]: Concurrent](
      resService: CountryService[F]
  ): HttpRoutes[F] = {
    object dsl extends Http4sDsl[F];
    import dsl._
      HttpRoutes.of[F] {
        case GET -> Root / "country" / code =>
          Ok(resService.byCode(code)
            .map(oc =>  oc.fold (
            s"No country found with code $code.")((c: Country) => c.asJson.spaces4)))

        case GET -> Root / "countries" =>
          resService.all.flatMap { st =>
            val stt = st.compile.toList.map(_.asJson) //how to use stream directly in the response?
            Ok(stt)
          }
      }
  }

  /** Our application as a resource. */
  def resServer[F[_]: Concurrent : Async: Console: Trace]: Resource[F,  Server] =
    resResCountryService.map {
      cs =>
        val routes = resRoutes(cs)
        Http4sExample.httpAppFrom(routes)
    } .flatMap { app =>
      Http4sExample.resServer(app)
    }

  /** Main method instantiates `F` to `IO` and `use`s our resource forever. */
  def run(args: List[String]): IO[ExitCode] =
    resServer[IO].useForever

}
