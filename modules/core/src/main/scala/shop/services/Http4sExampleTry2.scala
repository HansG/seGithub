// Copyright (c) 2018-2021 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package shop.services

import cats._
import cats.effect.std.Console
import cats.effect._
import cats.syntax.all._
import com.comcast.ip4s.IpLiteralSyntax
import fs2.Stream
import fs2.io.net.Network
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.middleware.{RequestLogger, ResponseLogger}
import org.http4s.server.{Router, Server}
import org.http4s.{HttpApp, HttpRoutes}
import skunk.codec.text.{bpchar, varchar}
import skunk.implicits._
import skunk.{Fragment, Query, Session, Void}
import natchez.Trace
import natchez.Trace.Implicits.noop

/**
  * A small but complete web service that serves data from the `world` database.
 * Note that the effect `F` is abstract throughout. So run this program and then try some requests:
  *
  *  curl -i http://localhost:8080/country/USA
  *  curl -i http://localhost:8080/country/foobar
  *
  */
object Http4sExample2 extends IOApp {

  /** A data model with a Circe `Encoder` */
  case class Country(code: String, name: String)
  object Country {
    implicit val encoderCountry: Encoder[Country] = deriveEncoder
  }

  /** A service interface and companion factory method. */
  trait CountryService[F[_]] {
    def byCode(code: String): F[Option[Country]]
    def all: Resource[F, Stream[F, Country]]
  }

  /** Given a `Session` we can create a `Countries` resource with pre-prepared statements. */
  def resCountryService[F[_]: Monad: MonadCancel[*[_], Throwable]](
      resSession: Resource[F, Session[F]]
  ): CountryService[F] = {

    def countryQuery[A](where: Fragment[A]): Query[A, Country] =
      sql"SELECT code, name FROM country $where".query((bpchar(3) ~ varchar).gmap[Country])

      new CountryService[F] {
        def byCode(code: String): F[Option[Country]] =
          resSession.use { sess =>
            sess.prepare(countryQuery(sql"WHERE code = ${bpchar(3)}")).use { psByCode =>
              psByCode.option(code)
            }
          }

        def all: F[Stream[F, Country]] =
          resSession.use { sess =>
            sess.prepare(countryQuery(Fragment.empty)).use { psAll =>
              psAll.stream(Void, 64)
            }
          }
      }
  }

  def resCountryService[F[_]: Monad: MonadThrow: Concurrent: Network: Console: Trace]
      : Resource[F, Session[F]] =
     Session.single[F](
      host = "localhost",
      port = 5432,
      user = "jimmy",
      password = Some("banana"),
      database = "world"
    )


  /** Resource yielding a pool of `CountryService`, backed by a single `Blocker` and `SocketGroup`. */
  def resResSession[F[_]: Concurrent: Network: Console: Trace]: Resource[F, Resource[F, Session[F]]] =
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

  /** Given a pool of `Countries` we can create an `HttpRoutes`. */
  def resRoutes[F[_]: Concurrent](
                                   countries: CountryService[F]
  ): HttpRoutes[F] = {
    object dsl extends Http4sDsl[F];
    import dsl._
      HttpRoutes.of[F] {
        case GET -> Root / "country" / code =>
          countries.byCode(code).flatMap {
            case Some(c) => Ok(c.asJson)
            case None    => NotFound(s"No country has code $code.")
          }

        case GET -> Root / "countries" =>
          countries.all.use { st =>
            val stt = st.compile.toList.map(_.asJson)
            Ok(stt)
          }
      }
  }

  def toHttpApp[F[_]: Async: Console](routes: HttpRoutes[F]): HttpApp[F] = {
    def addLoggers(http: HttpApp[F]): HttpApp[F] = {
      val httpReq = RequestLogger.httpApp(true, true)(http)
      ResponseLogger.httpApp(true, true)(httpReq)
    }

    addLoggers(Router("/" -> routes).orNotFound)
  }

  /** Given an `HttpApp` we can create a running `Server` resource. */
  def resServer[F[_]: Async](
      httpApp: HttpApp[F]
  ): Resource[F, Server] =
    EmberServerBuilder
      .default[F]
      .withHost(host"localhost")
      .withPort(port"8080")
      .withHttpApp(httpApp)
      .build

  /** Our application as a resource. */
  def resServer[F[_]: Async: Console: Trace]: Resource[F, Unit] =
    for {
      rrs    <- resResSession
      routes = resRoutes(rrs)
      app = toHttpApp(routes)
      _ <- resServer(app)
    } yield ()

  /** Main method instantiates `F` to `IO` and `use`s our resource forever. */
  def run(args: List[String]): IO[ExitCode] =
    resServer[IO].useForever

}
