package shop.http.routes

import cats.effect._
import cats.syntax.all._
import com.comcast.ip4s.{Host, IpLiteralSyntax, Port}
import derevo.cats.{eqv, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.syntax.EncoderOps
import io.estatico.newtype.macros.newtype
import monocle.Iso
import org.http4s.Method._
import org.http4s._
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.toMessageSyntax
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.middleware.{RequestLogger, ResponseLogger}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import shop.config.types.{HttpClientConfig, HttpServerConfig}
import shop.domain.ID._
import shop.domain.order.PaymentError
import shop.optics.IsUUID
import shop.resources.{MkHttpClient, MkHttpServer}

import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import cats.Monad
import cats.effect.kernel.Resource
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.{Router, Server}
import org.http4s.server.defaults.Banner
import shop.http.routes.MainX.ServerConfig
import upickle.default


object AppX extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    IO(println(""))
  }.as[ExitCode](ExitCode.Success)
}


@derive(decoder, encoder, eqv, show) //, uuid
@newtype
case class ProdId(value: UUID)

object ProdId {
  implicit val identityProdId: IsUUID[ProdId] = new IsUUID[ProdId] {
    val _UUID = Iso[UUID, ProdId](ProdId(_))(_.value)
  }
}

@derive(decoder, encoder, eqv, show)
@newtype
case class ProdName(value: String) {
  def toProd(prodId: ProdId): Prod =
    Prod(prodId, this)

  def toString1: String = { "ProdName:" + value }
}

@derive(decoder, encoder, eqv, show)
case class Prod(id : ProdId, name : ProdName)


trait Service[F[_]] {
  def findAll: F[List[Prod]]
  def create(name: ProdName): F[ProdId]
}


class ServiceTry extends Service[IO] {
  implicit val logger                       = Slf4jLogger.getLogger[IO]
  def findAll: IO[List[Prod]]              = IO.pure(List.empty)
  def create1(name: ProdName): IO[ProdId] = make[IO, ProdId]
  def create(name: ProdName): IO[ProdId] =
    Logger[IO].info(s"@server: name.toString= ${name.toString1}") >>
      make[IO, ProdId].flatTap(bi => IO(println(s"@server: ProdId=${bi.asJson.spaces2SortKeys}")))
}




object  ServiceTry {
  lazy val prodIdGen: Gen[ProdId] =  Gen.uuid.map(ProdId(_))

  lazy val prodNameGen : Gen[ProdName] = Gen.alphaStr.map(ProdName(_))

  lazy val prodGen: Gen[Prod] =
    for {
      i <- prodIdGen
      n <- prodNameGen
    } yield Prod(i, n)


  val bg                     = Gen.listOf(prodGen)
  val params: Gen.Parameters = Gen.Parameters.default.withSize(10)

  val nonEmptyPList: Gen[List[Prod]] =
    Gen
      .chooseNum(1, 10)
      .flatMap { n =>
        Gen.buildableOfN[List[Prod], Prod](n, prodGen)
      }

  def dataProds1: ServiceTry = new ServiceTry {
    override def findAll: IO[List[Prod]] =
      IO.pure(nonEmptyPList.pureApply(params, Seed.random(), 2))
  }
}

object MainX extends IOApp.Simple {

  override def run: IO[Unit] = runTestS
  // override def run: IO[Unit] = runFst


  implicit val logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  val loggers: HttpApp[IO] => HttpApp[IO] = {
    { http: HttpApp[IO] =>
      RequestLogger.httpApp(true, true)(http)
    } andThen { http: HttpApp[IO] =>
      ResponseLogger.httpApp(true, true)(http)
    }
  }

  case class ServerConfig( host: Host, port: Port )

  val httpsc = ServerConfig(
    host = host"127.0.0.1",
    port = port"8080"
  )

  final case class ProdRoutes[F[_]: Monad]( service: Service[F] ) extends Http4sDsl[F] {
    private[routes] val prefixPath = "/prods"

    private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
      case GET -> Root => Ok(service.findAll)
    }

    val routes: HttpRoutes[F] = Router(
      prefixPath -> httpRoutes
    )
  }


  def newEmber[F[_] : Async](cfg : ServerConfig, httpApp : HttpApp[F]) = EmberServerBuilder
    .default[F]
    .withHost(cfg.host)
    .withPort(cfg.port)
    .withHttpApp(httpApp)
    .build


  def runTestS = {
    val myRoutes = ProdRoutes[IO](ServiceTry.dataProds1).routes
    val prodApp = loggers(Router(version.v1 -> myRoutes).orNotFound)
    val server =  newEmber[IO](httpsc, prodApp)
    server.useForever
  }

}

//Client ------------------------------------------------------

@newtype case class CliUri(value: NonEmptyString)
@newtype case class CliConfig(uri: CliUri)


trait HttpClient {
  def process: IO[List[Prod]]
  def processX(prodP: ProdName): IO[Prod]
}

object HttpClient {
  def make(cfg: CliConfig, client: Client[IO]): HttpClient =
    new HttpClient with Http4sClientDsl[IO] {
      def process: IO[List[Prod]] =
        Uri.fromString(cfg.uri.value + "/v1/prods").liftTo[IO].flatMap { uri =>
          client.run(GET(uri)).use { resp =>
            resp.status match {
              case Status.Ok | Status.Conflict =>
                resp.asJsonDecode[List[Prod]]
              case st =>
                PaymentError(
                  Option(st.reason).getOrElse("unknown")
                ).raiseError[IO, List[Prod]]
            }
          }
        }

      def processX(prodP: ProdName): IO[Prod] =
        Uri.fromString(cfg.uri.value + "/v1").liftTo[IO].flatMap { uri =>
          client.run(POST(prodP, uri / "prodsX")).use { resp =>
            resp.status match {
              case Status.Created | Status.Conflict =>
                resp.asJsonDecode[Prod]
              case st =>
                PaymentError(
                  Option(st.reason).getOrElse("unknown")
                ).raiseError[IO, Prod]
            }
          }
        }
    }
}

object MainY extends IOApp.Simple {
  import MainX.logger

  override def run: IO[Unit] = runClient

  def useClient(cl: HttpClient) = cl.process.flatMap { li =>
    IO.println(li)
  }

  def useClientA(cl: HttpClient, bn: ProdName) =
    cl.processX(bn).flatMap { li =>
      IO.println(li)
    }

  def useClient1(cl: HttpClient) =
    fs2.Stream.fixedRate[IO](FiniteDuration(1, TimeUnit.SECONDS)).take(10).evalMap(_ => useClient(cl)).compile.drain


  case class  ClientConfig(timeout: FiniteDuration, idleTimeInPool: FiniteDuration)


  case class AppConfigX(httpClientConfig:  ClientConfig, paymentConfig :CliConfig, httpServerConfig:  ServerConfig)
  import eu.timepit.refined.auto._
  val clientC =  AppConfigX(
     ClientConfig(
      timeout = 60.seconds,
      idleTimeInPool = 30.seconds
    ),
    CliConfig(CliUri("http://localhost:8080")),
     ServerConfig(
      host = host"127.0.0.1",
      port = port"8080"
    )
  )

  def newEmber[F[_]:Async](c: ClientConfig): Resource[F, Client[F]] =
    EmberClientBuilder
      .default[F]
      .withTimeout(c.timeout)
      .withIdleTimeInPool(c.idleTimeInPool)
      .build

  def runClient = IO(clientC).flatMap { cfg =>
    Logger[IO].info(s"Loaded config $cfg") >>
       newEmber[IO](cfg.httpClientConfig)
        .map { client =>
          HttpClient.make(cfg.paymentConfig, client)
        }
        .use { cl =>
          useClientA(cl, ProdName("Sepp"))
        // useClient(cl)
        }
  }

}
