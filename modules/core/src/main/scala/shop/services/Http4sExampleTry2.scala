// Copyright (c) 2018-2021 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package shop.services

import cats._
import cats.effect.std.Console
import cats.effect._
import cats.syntax.all._
import fs2.Stream
import fs2.io.net.Network
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Server
import org.http4s.HttpRoutes
import skunk.codec.text.{bpchar, varchar}
import skunk.implicits._
import skunk.{Fragment, Query, Session, Void}
import natchez.Trace
import natchez.Trace.Implicits.noop
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
  * A small but complete web service that serves data from the `world` database.
 * Note that the effect `F` is abstract throughout. So run this program and then try some requests:
  *
  *  curl -i http://localhost:8080/country/USA
  *  curl -i http://localhost:8080/country/foobar
  *
  */
object Http4sExample2 extends Http4sExample with IOApp  {


  /** A service interface and companion factory method. */
  trait CountryService[F[_]] {
    def byCode(code: String): F[Option[Country]]
    def all: F[Stream[F, Country]]
  }


  /** Given a `Session` we can create a `Countries` resource with pre-prepared statements. */
  def countryServiceFrom[F[_]: Monad: MonadCancel[*[_], Throwable] : Logger](
      resSession: Resource[F, Session[F]]
  ): CountryService[F] = {

    def countryQuery[A](where: Fragment[A]): Query[A, Country] =
      sql"SELECT code, name FROM country $where".query((bpchar(3) ~ varchar).gmap[Country])

      new CountryService[F] {
        def byCode(code: String): F[Option[Country]] =
          resSession.use { sess =>
            sess.prepare(countryQuery(sql"WHERE code = ${bpchar(3)}")).flatMap  { psByCode =>
              psByCode.option(code)
            }
          }.onError {
            case err =>
              //Logger[F].error(
              Monad[F].pure(println(
                s"Failed on: ${err.toString} \n ${err.getCause}"
              ))
          }

        def all:  F[Stream[F, Country]] =
          resSession.use { sess =>
           sess.prepare(countryQuery(Fragment.empty)).flatMap { prepQ =>
              Monad[F].pure( prepQ.stream(Void, 64)
              )
            }
          }.onError {
            case ex:Exception  => Monad[F].pure(println(ex.toString))
            case e => Monad[F].pure(println(e.toString))
          }

      }
  }

  
  /** Resource yielding a pool of `CountryService`, backed by a single `Blocker` and `SocketGroup`. */
  def resResSession[F[_]: Concurrent: Network: Console: Trace: Temporal]: Resource[F, Resource[F, Session[F]]] =
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
  def routesFrom[F[_]: Concurrent](
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
          countries.all.flatMap { st =>
            val stt = st.compile.toList.map(_.asJson)
            Ok(stt)
          }
      }
  }



  /** Our application as a resource. */
  def resServer[F[_]: Async: Console: Trace : Logger]: Resource[F, Server] =
    resResSession.map { rs =>
      val cs = countryServiceFrom(rs)
      val r = routesFrom(cs)
      httpAppFrom(r)
    } flatMap { app =>
      resServer(app)
    }

  implicit val logger = Slf4jLogger.getLogger[IO]

  /** Main method instantiates `F` to `IO` and `use`s our resource forever. */
  def run(args: List[String]): IO[ExitCode] =
    resServer[IO].useForever

}
