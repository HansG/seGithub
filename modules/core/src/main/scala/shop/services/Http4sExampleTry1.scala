// Copyright (c) 2018-2021 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package shop.services

import cats._
import cats.effect._
import cats.effect.std.Console
import cats.syntax.all._
import fs2.Stream
import fs2.io.net.Network
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._
import natchez.Trace
import natchez.Trace.Implicits.noop
import org.http4s.HttpRoutes
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import skunk.codec.text.{ bpchar, varchar }
import skunk.implicits._
import skunk.{ Fragment, Query, Session, Void }

object Http4sExampleTry1 extends IOApp {

  /** A data model with a Circe `Encoder` */
  case class Country(code: String, name: String)

  object Country {
    implicit val encoderCountry: Encoder[Country] = deriveEncoder
  }

  /** A service interface and companion factory method. */
  trait CountryService[F[_]] {
    def byCode(code: String): F[Option[Country]]
    def all: F[Stream[F, Country]]
  }

  /** Given a `Session` we can create a `Countries` resource with pre-prepared statements. */
  def resCountryService[F[_]: Monad: MonadCancel[*[_], Throwable]](
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

  /** Resource yielding a pool of `CountryService`, backed by a single `Blocker` and `SocketGroup`. */
  def resResCountryService[F[_]: Concurrent: Network: Console: Trace: Temporal]
      : Resource[F, Resource[F, CountryService[F]]] =
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
      .map(rs => resCountryService(rs))

  /** Given a pool of `Countries` we can create an `HttpRoutes`. */
  def resRoutes[F[_]: Concurrent](
      resService: Resource[F, CountryService[F]]
  ): Resource[F, HttpRoutes[F]] = {
    object dsl extends Http4sDsl[F];
    import dsl._
    resService.map { countries =>
      HttpRoutes.of[F] {
        case GET -> Root / "country" / code =>
          countries.byCode(code).flatMap {
            case Some(c) => Ok(c.asJson)
            case None    => NotFound(s"No country has code $code.")
          }

        case GET -> Root / "countries" =>
          countries.all.flatMap { st =>
            val stt = st.compile.toList.map(_.asJson) //how to use stream directly in the response?
            Ok(stt)
          }
      }
    }
  }

  /** Our application as a resource. */
  def resServer[F[_]: Async: Console: Trace]: Resource[F, Unit] =
    for {
      rs     <- resResCountryService
      routes <- resRoutes(rs)
      app = Http4sExample.httpAppFrom(routes)
      _ <- Http4sExample.resServer(app)
    } yield ()

  /** Main method instantiates `F` to `IO` and `use`s our resource forever. */
  def run(args: List[String]): IO[ExitCode] =
    resServer[IO].useForever

}
