package shop.http.routes

import cats.effect._
import ciris.env
import com.comcast.ip4s.Literals.port
import org.http4s.Method._
import org.http4s._
import org.http4s.circe.{JsonDecoder, toMessageSyntax}
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.dsl.io._
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Router
import org.http4s.server.middleware.{RequestLogger, ResponseLogger}
import org.http4s.syntax.literals._
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import shop.config.AppEnvironment
import shop.config.AppEnvironment.{Prod, Test}
import shop.config.types.{HttpClientConfig, HttpServerConfig, PaymentConfig, PaymentURI}
import shop.domain.ID
import shop.domain.ID._
import shop.domain.brand._
import shop.domain.order.{PaymentError, PaymentId}
import shop.domain.payment.Payment
import shop.generators._
import shop.http.clients.PaymentClient
import shop.http.routes.BrandRoutesSuiteX.{dataBrands, dataBrands1}
import shop.modules.HttpClients
import shop.programs.CheckoutSuite.F
import shop.resources.{MkHttpClient, MkHttpServer}
import shop.services.Brands
import suite.HttpSuite
import weaver.TestOutcome

import java.util.UUID

object AppX extends  IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    IO(println(""))
  }.as[ExitCode](ExitCode.Success)
}


object BrandRoutesSuiteX extends HttpSuite{
  val bg = Gen.listOf(brandGen)
  val params: Gen.Parameters = Gen.Parameters.default.withSize(10)

  val nonEmptyBList: Gen[List[Brand]] =
    Gen.chooseNum(1, 10)
      .flatMap { n =>
        Gen.buildableOfN[List[Brand], Brand](n, brandGen)
      }

  def dataBrands1 = new TestBrandsX {
    override def findAll: IO[List[Brand]] =
      //IO.pure(bg.pureApply(params, Seed.random(),5))
      IO.pure(nonEmptyBList.pureApply(params, Seed.random(),2))
  }

  def dataBrands(brands: List[Brand]) = new TestBrandsX {
    override def findAll: IO[List[Brand]] =
      IO.pure(brands)
  }

  def failingBrands(brands: List[Brand]) = new TestBrandsX {
    override def findAll: IO[List[Brand]] =
      IO.raiseError(DummyError) *> IO.pure(brands)
  }


  test("GET brands succeeds") {
    forall(Gen.listOf(brandGen)) { b =>
      IO(println(b)) >>  IO.pure(success)
     }
  }

/*
  test("GET brands succeeds") {
    forall(Gen.listOf(brandGen)) { b =>
      val req    = GET(uri"/brands")
      val routes = BrandRoutes[IO](dataBrands(b)).routes
      expectHttpBodyAndStatus(routes, req)(b, Status.Ok)
    }
  }

  test("GET brands fails") {
    forall(Gen.listOf(brandGen)) { b =>
      val req    = GET(uri"/brands")
      val routes = BrandRoutes[IO](failingBrands(b)).routes
      expectHttpFailure(routes, req)
    }
  }
*/

}


object StartExX extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = BrandRoutesSuiteX.run(List("a", "b")) { (to:TestOutcome) =>
    IO(println(to))
  }.as[ExitCode](ExitCode.Success)
}

object MainX extends IOApp.Simple {

  implicit val logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  private val loggers: HttpApp[F] => HttpApp[F] = {
    { http: HttpApp[F] =>
      RequestLogger.httpApp(true, true)(http)
    } andThen { http: HttpApp[F] =>
      ResponseLogger.httpApp(true, true)(http)
    }
  }

  override def run: IO[Unit] =
    ConfigX.dcfg.flatMap { cfg =>
      Logger[IO].info(s"Loaded config $cfg") >>
        MkHttpClient[F]
          .newEmber(cfg.httpClientConfig)
          .map { client =>
            val brandRoutes =
              BrandRoutes[IO](dataBrands1).routes
            //  val brandRoutes = BrandRoutes[IO](failingBrands(List(Brand(BrandId(UUID.randomUUID()), BrandName("brand1"))))).routes
            val brandApp = loggers(Router(version.v1 -> brandRoutes).orNotFound)
            val clients  = PaymentClientX.make(cfg.paymentConfig, client)

            cfg.httpServerConfig -> brandApp
          }
          .flatMap {
            case (cfg, httpApp) =>
              MkHttpServer[IO].newEmber(cfg, httpApp)
          }
          .useForever
    }

}
case class AppConfigX(httpClientConfig: HttpClientConfig, paymentConfig :PaymentConfig,  httpServerConfig: HttpServerConfig)

import com.comcast.ip4s._
import scala.concurrent.duration._
import eu.timepit.refined.auto._
import cats.syntax.all._

trait PaymentClientX {
  def process: IO[List[Brand]]
}

object PaymentClientX {
  def make( cfg: PaymentConfig, client: Client[IO] ): PaymentClientX =
    new PaymentClientX with Http4sClientDsl[IO] {
      def process: IO[List[Brand]] =
        Uri.fromString(cfg.uri.value + "/brands").liftTo[IO].flatMap { uri =>
          client.run(GET(uri)).use { resp =>
            resp.status match {
              case Status.Ok | Status.Conflict =>
                resp.asJsonDecode[List[Brand]]
              case st =>
                PaymentError(
                  Option(st.reason).getOrElse("unknown")
                ).raiseError[IO, List[Brand]]
            }
          }
        }
    }
}





object ConfigX {
  val dcfg = IO(defaultC)

  val defaultC = AppConfigX(
    HttpClientConfig(
      timeout = 60.seconds,
      idleTimeInPool = 30.seconds
    ),
    PaymentConfig(PaymentURI("http://localhost:8080")),
  HttpServerConfig(
      host = host"127.0.0.1",
      port = port"8080"
    )
  )
  // Ciris promotes configuration as code
  def load: IO[AppConfigX] =
    env("SC_APP_ENV")
      .as[AppEnvironment]
      .map {
        case Test => defaultC
        case Prod => defaultC

      }
      .load[IO]

}

protected class TestBrandsX extends Brands[IO] {
  def create(name: BrandName): IO[BrandId] = make[IO, BrandId]
  def findAll: IO[List[Brand]]             = IO.pure(List.empty)
}
