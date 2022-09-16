package shop.http.routes

import cats.effect._
import cats.syntax.all._
import ciris.env
import com.comcast.ip4s.IpLiteralSyntax
import com.comcast.ip4s.Literals.port
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.syntax.EncoderOps
import org.http4s.Method._
import org.http4s._
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.{JsonDecoder, toMessageSyntax}
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.dsl.io._
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Router
import org.http4s.server.middleware.{RequestLogger, ResponseLogger}
import org.http4s.syntax.literals._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import shop.config.AppEnvironment
import shop.config.AppEnvironment.{Prod, Test}
import shop.config.types.{HttpClientConfig, HttpServerConfig, PaymentConfig, PaymentURI}
import shop.domain.{ID, brand}
import shop.domain.ID._
import shop.domain.brand._
import shop.domain.order.{PaymentError, PaymentId}
import shop.domain.payment.Payment
import shop.generators._
import shop.http.clients.PaymentClient
import shop.http.routes.BrandRoutesSuiteTry0.{dataBrands, dataBrands1}
import shop.http.routes.admin.AdminBrandRoutesTry0
import shop.modules.HttpClients
import shop.resources.{MkHttpClient, MkHttpServer}
import shop.services.Brands
import skunk.syntax.id
import suite.HttpSuite
import weaver.TestOutcome

import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object AppX extends  IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    IO(println(""))
  }.as[ExitCode](ExitCode.Success)
}


object BrandRoutesSuiteTry0 extends HttpSuite{
  val bg = Gen.listOf(brandGen)
  val params: Gen.Parameters = Gen.Parameters.default.withSize(10)

  val nonEmptyBList: Gen[List[Brand]] =
    Gen.chooseNum(1, 10)
      .flatMap { n =>
        Gen.buildableOfN[List[Brand], Brand](n, brandGen)
      }

  def dataBrands1 : TestBrandsX = new TestBrandsX {
    override def findAll: IO[List[Brand]] =
      //IO.pure(bg.pureApply(params, Seed.random(),5))
      IO.pure(nonEmptyBList.pureApply(params, Seed.random(),2))
  }

  def dataBrands(brands: List[Brand]) : TestBrandsX = new TestBrandsX {
    override def findAll: IO[List[Brand]] =
      IO.pure(brands)
  }

  def failingBrands(brands: List[Brand]) : TestBrandsX = new TestBrandsX {
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
  override def run(args: List[String]): IO[ExitCode] = BrandRoutesSuiteTry0.run(List("a", "b")) { (to:TestOutcome) =>
    IO(println(to))
  }.as[ExitCode](ExitCode.Success)
}

object MainX extends IOApp.Simple {

  implicit val logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  val loggers: HttpApp[IO] => HttpApp[IO] = {
    { http: HttpApp[IO] =>
      RequestLogger.httpApp(true, true)(http)
    } andThen { http: HttpApp[IO] =>
      ResponseLogger.httpApp(true, true)(http)
    }
  }

  override def run: IO[Unit] = runTestS
 // override def run: IO[Unit] = runFst

   def runScd: IO[Unit] =
    ConfigX.dcfg.flatMap { cfg =>
      Logger[IO].info(s"Loaded config $cfg") >>
        MkHttpClient[IO]
          .newEmber(cfg.httpClientConfig)
          .map { client =>
            val brandRoutes =
              BrandRoutes[IO](dataBrands1).routes
            //  val brandRoutes = BrandRoutes[IO](failingBrands(List(Brand(BrandId(UUID.randomUUID()), BrandName("brand1"))))).routes
            val brandApp = loggers(Router(version.v1 -> brandRoutes).orNotFound)
            val clientb  = BrandClientX.make(cfg.paymentConfig, client)

            (clientb, cfg.httpServerConfig -> brandApp)
          }
          .flatMap {
            case (clientb,(cfg, httpApp)) =>
              MkHttpServer[IO].newEmber(cfg, httpApp).map(server => (clientb, server))
          }.use {
          case (clientb, server) => {
            clientb.process.flatMap( IO.println(_))
            //pclient Aufrufe!!!!!!
          //  IO.never
          }

        }

    }

  val httpsc = HttpServerConfig(
    host = host"127.0.0.1",
    port = port"8080"
  )

  def runTestS   =
    IO(httpsc).flatTap { cfg =>
      Logger[IO].info(s"Loaded config $cfg")
    }.map {  cfg  =>
      val myRoutes = BrandRoutes[IO](dataBrands1).routes
      val brandApp = loggers(Router(version.v1 -> myRoutes ).orNotFound)

      (cfg, brandApp)
    }.flatMap {
      case (cfg, httpApp) =>
        val server = MkHttpServer[IO].newEmber(cfg, httpApp)
        server.useForever
    }



//2022XX
   def runFst: IO[Unit] =
    ConfigX.dcfg.flatMap { cfg =>
      Logger[IO].info(s"Loaded config $cfg") >>
        MkHttpClient[IO]
          .newEmber(cfg.httpClientConfig)
          .map { client =>   //wird hier nicht verwendet .... newEmber nur wegen lift zu Ressource
             //  val brandRoutes = BrandRoutes[IO](failingBrands(List(Brand(BrandId(UUID.randomUUID()), BrandName("brand1"))))).routes
            val brandRoutes = BrandRoutes[IO](dataBrands1).routes
            val adminRoutes   = AdminBrandRoutesTry0[IO](dataBrands1).routes

            val myRoutes = brandRoutes  <+> adminRoutes

            val brandApp = loggers(Router(version.v1 -> myRoutes ).orNotFound)
       //     val clients  = BrandClientX.make(cfg.paymentConfig, client)

            cfg.httpServerConfig -> brandApp
          }
          .flatMap {
            case (cfg, httpApp) =>
              MkHttpServer[IO].newEmber(cfg, httpApp)
          }
          .useForever
    }


}
object MainY extends IOApp.Simple {
  import MainX.logger

  override def run: IO[Unit] = runClient

  def useClient(cl : BrandClientX) = cl.process.flatMap{  li => IO.println(li)}

  def useClientA(cl : BrandClientX, bn : BrandName) =
    cl.processX(bn).flatMap{  li => IO.println(li )}

  def useClient1(cl : BrandClientX) =
    fs2.Stream.fixedRate[IO](FiniteDuration(1, TimeUnit.SECONDS)).take(10).evalMap(_ => useClient(cl)).compile.drain


  def runClient =     ConfigX.dcfg.flatMap { cfg =>
    Logger[IO].info(s"Loaded config $cfg") >>
      MkHttpClient[IO]
        .newEmber(cfg.httpClientConfig)
        .map { client =>  BrandClientX.make(cfg.paymentConfig, client)  }
        .use { cl =>
          useClientA(cl, BrandName("Sepp"))
          // useClient(cl)
        }
  }

}


case class AppConfigX(httpClientConfig: HttpClientConfig, paymentConfig :PaymentConfig,  httpServerConfig: HttpServerConfig)

import com.comcast.ip4s._
import scala.concurrent.duration._
import eu.timepit.refined.auto._
import cats.syntax.all._

trait BrandClientX {
  def process: IO[List[Brand]]
  def processX(brandP : BrandName): IO[Brand]
}

object BrandClientX {
  def make( cfg: PaymentConfig, client: Client[IO] ): BrandClientX =
    new BrandClientX with Http4sClientDsl[IO] {
      def process: IO[List[Brand]] =
        Uri.fromString(cfg.uri.value + "/v1/brands").liftTo[IO].flatMap { uri =>
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

      def processX(brandP : BrandName): IO[Brand] =
        Uri.fromString(cfg.uri.value + "/v1").liftTo[IO].flatMap { uri =>
          client.run(   POST(brandP, uri/"brandsX")
          ).use { resp =>
            resp.status match {
              case Status.Created | Status.Conflict =>
                 resp.asJsonDecode[Brand]
              case st =>
                PaymentError(
                  Option(st.reason).getOrElse("unknown")
                ).raiseError[IO, Brand]
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
  implicit val logger = Slf4jLogger.getLogger[IO]
  def findAll: IO[List[Brand]]             = IO.pure(List.empty)
  def create1(name: BrandName): IO[BrandId] =   make[IO, BrandId]
  def create(name: BrandName): IO[BrandId] = Logger[IO].info(s"@server: name.toString= ${name.toString1}") >>
      make[IO, BrandId].flatTap(bi => IO(println(s"@server: BrandId=${bi.asJson.spaces2SortKeys}")))



}
