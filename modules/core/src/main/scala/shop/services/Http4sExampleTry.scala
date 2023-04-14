// Copyright (c) 2018-2021 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package shop.services

import cats.effect.std.Console
import cats.effect._
import com.comcast.ip4s.IpLiteralSyntax
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.middleware.{RequestLogger, ResponseLogger}
import org.http4s.server.{Router, Server}
import org.http4s.{HttpApp, HttpRoutes}
import fs2.Stream
import fs2.io.net.Network
import cats.syntax.all._
import cats._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
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
 *
 *  #testtool mit json Ausgabe (und Eingabe) https://reqbin.com/
  *
  */
object Http4sExample   {




  def httpAppFrom[F[_]: Async: Console](routes: HttpRoutes[F]): HttpApp[F] = {
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

  /*
      val resSession = Session.single[F](
        host = "localhost",
        port = 5432,
        user = "jimmy",
        password = Some("banana"),
        database = "world"
      )
  */



  }


