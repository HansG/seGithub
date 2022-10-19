// Copyright (c) 2018-2021 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package shop.services

import cats._
import cats.effect.{Resource, _}
import cats.effect.std.Console
import cats.syntax.all._
import com.comcast.ip4s.IpLiteralSyntax
import fs2.Stream
import fs2.io.net.{Network, SocketGroup}
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._
import natchez.Trace.Implicits.noop
import natchez.{EntryPoint, Trace}
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.{Router, Server}
import skunk.{Fragment, Query, Session, Void}
import skunk.codec.text.{bpchar, varchar}
import skunk.implicits._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.{RequestLogger, ResponseLogger}
import skunk.util.Recycler

import scala.concurrent.ExecutionContext.global

/**
  * A small but complete web service that serves data from the `world` database and accumulates
  * request traces to a tracing back-end. Note that the effect `F` is abstract throughout. This
  * example uses the Jaeger trace collector that's provided as part of the project's docker-compose
  * file. So run this program and then try some requests:
  *
  *  curl -i http://localhost:8080/country/USA
  *  curl -i http://localhost:8080/country/foobar
  *
  * And Then go to http://localhost:16686/search and select the `skunk-http4s-example` service and
  * click search to see request traces. You will see that the first trace authenticates and prepares
  * statements, but subsequent requests (on the same session, which is likely the case if you're
  * monkey-testing) do not.
  */
object Http4sExample extends IOApp {

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

  /**
    * A refinement that provides a handle to the underlying session. We will use this to implement
    * our pool recycler.
    */
  trait PooledCountries[F[_]] extends CountryService[F] {
    def session: Session[F]
  }

  def pooledCountriesRecycler[F[_]: Monad]: Recycler[F, PooledCountries[F]] =
    Session.Recyclers.minimal[F].contramap(_.session)

  /** Given a `Session` we can create a `Countries` resource with pre-prepared statements. */
  def countriesFromSession[F[_]: Trace: Monad: MonadCancel[*[_], Throwable]](
      pool: Resource[F, Session[F]]
  ): Resource[F, CountryService[F]] = {

    def countryQuery[A](where: Fragment[A]): Query[A, Country] =
      sql"SELECT code, name FROM country $where".query((bpchar(3) ~ varchar).gmap[Country])

    pool.map { sess =>
      new CountryService[F] {
        def byCode(code: String): F[Option[Country]] =
          Trace[F].span(s"""Country.byCode("$code")""") {
            sess.prepare(countryQuery(sql"WHERE code = ${bpchar(3)}")).use { psByCode =>
              psByCode.option(code)
            }
          }

        def all: Resource[F, Stream[F, Country]] =
          sess.prepare(countryQuery(Fragment.empty)).map { psAll =>
            psAll.stream(Void, 64)
          }

      }

    }

  }

  /**
    * Given a `SocketGroup` we can construct a session resource, and from that construct a
    * `Countries` resource.
    */
//   def countriesFromSocketGroup[F[_]: Concurrent: ContextShift: Trace](
//     socketGroup: SocketGroup[F]
//   ): Resource[F, PooledCountries[F]] =
//     Session.fromSocketGroup(
//       host         = "localhost",
//       user         = "jimmy",
//       database     = "world",
//       password     = Some("banana"),
//       socketGroup  = socketGroup,
//       sslOptions   = None,
//       parameters   = Session.DefaultConnectionParameters
//     ).flatMap(countriesFromSession(_))
  //zuvor countriesFromSocketGroup
  def singleSession[F[_]: Monad: MonadThrow: Concurrent: Network: Console: Trace]: Resource[F, CountryService[F]] = {
    val rs = Session.single[F](
      host = "localhost",
      port = 5432,
      user = "jimmy",
      password = Some("banana"),
      database = "world"
    )
    countriesFromSession(rs)
  }

  /** Resource yielding a pool of `Countries`, backed by a single `Blocker` and `SocketGroup`. */
//   def pool[F[_]: Concurrent: ContextShift: Trace]: Resource[F, Resource[F, Countries[F]]] =
//     for {
//       b  <- Blocker[F]
//       sg <- fs2.io.net.SocketGroup[F[_]](b)
//       pc  = countriesFromSocketGroup(sg)
//       r  <- Pool.of(pc, 10)(pooledCountriesRecycler)
//     } yield r.widen // forget we're a PooledCountries
  def resResService[F[_]: Concurrent: Network: Console: Trace]: Resource[F, Resource[F, CountryService[F]]] =
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
      .map(rs => countriesFromSession(rs))

  /** Given a pool of `Countries` we can create an `HttpRoutes`. */
  def resRoutes[F[_]: Concurrent: Trace](
      pool: Resource[F, CountryService[F]]
  ): Resource[F, HttpRoutes[F]] = {
    object dsl extends Http4sDsl[F];
    import dsl._
    pool.map { countries =>
      HttpRoutes.of[F] {
        case GET -> Root / "country" / code =>
          Trace[F].put("country" -> code) *> // add param to current span
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
  }



  def  httpApp[F[_]: Async: Console: Trace]( r : HttpRoutes[F]) :  HttpApp[F]  = {
    def addLoggers[F[_]: Async](http: HttpApp[F]): HttpApp[F] = {
      val httpReq = RequestLogger.httpApp(true, true)(http)
      ResponseLogger.httpApp(true, true)(httpReq)
    }

    addLoggers(Router("/" -> r).orNotFound)
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
  def runR[F[_]: Async: Console: Trace]: Resource[F, Unit] =
    for {
      rrs <- resResService
      routes <-  resRoutes(rrs)
      app =  httpApp(routes)
      _  <- resServer(app)
    } yield ()

  /** Main method instantiates `F` to `IO` and `use`s our resource forever. */
  def run(args: List[String]): IO[ExitCode] =
    runR[IO].use(_ => IO.never)

}
