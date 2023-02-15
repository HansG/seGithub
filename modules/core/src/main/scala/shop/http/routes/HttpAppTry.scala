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
import org.http4s.circe.{JsonDecoder, toMessageSyntax}
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.middleware.{RequestLogger, ResponseLogger}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.util.UUID
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import cats.{Monad, MonadThrow}
import cats.data.Kleisli
import cats.effect.kernel.Resource
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.{Router, Server}
import upickle.default
import eu.timepit.refined.auto._
import io.circe.Decoder
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import org.http4s.util.StringWriter
import shop.domain.order.PaymentError

object HttAppTry extends IOApp.Simple {

  object Domain {
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
    }

    @derive(decoder, encoder, eqv, show)
    case class Prod(id: ProdId, name: ProdName)

    trait IsUUID[A] {
      def _UUID: Iso[UUID, A]
    }
    object IsUUID {
      def apply[A: IsUUID]: IsUUID[A] = implicitly

      implicit val identityUUID: IsUUID[UUID] = new IsUUID[UUID] {
        val _UUID = Iso[UUID, UUID](identity)(identity)
      }
    }

    def make[F[_]: Sync, T: IsUUID] = Sync[F].delay(UUID.randomUUID()).map(IsUUID[T]._UUID.get(_))
  }

  import Domain._

  trait Service[F[_]] {
    def findAll: F[List[Prod]]
    def create(name: ProdName): F[ProdId]
  }

  class AMockService extends Service[IO] {
    implicit val logger                     = Slf4jLogger.getLogger[IO]
    def findAll: IO[List[Prod]]             = IO.pure(List.empty)
    def create1(name: ProdName): IO[ProdId] = make[IO, ProdId]
    def create(name: ProdName): IO[ProdId] =
      Logger[IO].info(s"@server.create Prod:name= ${name}") >>
        make[IO, ProdId].flatTap(bi => IO(println(s"@server: ProdId=${bi.asJson.spaces2SortKeys}")))
  }

  object AMockService {
    lazy val prodIdGen: Gen[ProdId] = Gen.uuid.map(ProdId(_))

    lazy val prodNameGen: Gen[ProdName] = Gen.alphaStr.map(ProdName(_))

    lazy val prodGen: Gen[Prod] =
      for {
        i <- prodIdGen
        n <- prodNameGen
      } yield Prod(i, n)

    val bg = Gen.listOf(prodGen)

    val nonEmptyPList: Gen[List[Prod]] =
      Gen
        .chooseNum(1, 10)
        .flatMap { n =>
          Gen.buildableOfN[List[Prod], Prod](n, prodGen)
        }

    def genMockService: AMockService = new AMockService {
      val params: Gen.Parameters = Gen.Parameters.default.withSize(10)
      override def findAll: IO[List[Prod]] =
        IO.pure(nonEmptyPList.pureApply(params, Seed.random(), 2))
    }
  }

  import shop.ext.http4s.refined._

  case class ServiceAtHttpApp(service: Service[IO]) extends Http4sDsl[IO] {
    lazy val httpRoutes: HttpRoutes[IO] = HttpRoutes.of[IO] {
      case GET -> Root => Ok(service.findAll)
      case req @ POST -> Root =>
        req.decodeR[ProdName] { pn =>
          service.create(pn).flatMap { id => //.toDomain
            Created(Prod(id, pn))
            // Created(JsonObject.singleton("brand_id", id.asJson))
          }
        }
    }

    def apply =  {
      val satrr: HttpRoutes[IO] = Router(UriConfig.pathPrefix -> httpRoutes)
      val app                   = Router(UriConfig.vers -> satrr).orNotFound

      def addLoggers[F[_]: Async](http: HttpApp[F]): HttpApp[F] = {
        val httpReq = RequestLogger.httpApp(true, true)(http)
        ResponseLogger.httpApp(true, true)(httpReq)
      }
      addLoggers(app)
    }
  }

  trait UriConfigI {
    val host: Host = host"127.0.0.1"
    val port: Port = port"8080"
    val vers       = version.v1
    val pathPrefix = "/prods"
    val uri : IO[Uri]
  }

  object UriConfig extends UriConfigI{

    def apply(hostp: Host = host, portp: Port = port): IO[Uri] = {
      val uris = s"http://$host:$port"
      Uri.fromString(uris + vers + pathPrefix).liftTo[IO]
      //alternativ:
      Uri.fromString(uris + vers).liftTo[IO].map(_ / "prods")
    }

    override val uri: IO[Uri] = apply()
  }


  def httpAppAtServer[F[_]: Async](httpApp: HttpApp[F]) =
    EmberServerBuilder
      .default[F]
      .withHost(UriConfig.host)
      .withPort(UriConfig.port)
      .withHttpApp(httpApp)
      .build

  def start = {
    val server = httpAppAtServer[IO]( ServiceAtHttpApp(AMockService.genMockService).apply )
    server.useForever
  }

  override def run: IO[Unit] = start
  // override def run: IO[Unit] = runFst

  /*object AppX extends IOApp {
    override def run(args: List[String]): IO[ExitCode] = {
      IO(println(""))
    }.as[ExitCode](ExitCode.Success)
  }*/

}

//Client ------------------------------------------------------

object HttAppClientTry extends IOApp.Simple {
  import HttAppTry.{UriConfigI, UriConfig, Domain }
  import Domain._

  trait ClientApi {
    def getProds: IO[List[Prod]]
    def postProds(prodP: ProdName): IO[Prod]
  }

  case class EmberClientAtServer(client: Client[IO], uriConfig: UriConfigI) extends ClientApi with Http4sClientDsl[IO] {

    def statusToString(ex: Status) = {
      val writer = new StringWriter
      ex.render(writer)
      writer.result
    }

    def getProds: IO[List[Prod]] =
      uriConfig.uri.flatMap {

        uri =>
        client.run(GET(uri)).use { resp =>
          resp.status match {
            case Status.Ok | Status.Conflict =>
              resp.asJsonDecode[List[Prod]]
            case ex =>
              PaymentError(
                Option(statusToString(ex)).getOrElse("unknown")
              ).raiseError[IO, List[Prod]]
          }
        }
      }

    def postProds(pn: ProdName): IO[Prod] =
      uriConfig.uri flatMap { uri =>
        client.run(POST(pn, uri)).use { resp =>
          resp.status match {
            case Status.Created | Status.Conflict =>
              resp.asJsonDecode[Prod]
            case st =>
              PaymentError(
                Option(statusToString(st)).getOrElse("unknown")
              ).raiseError[IO, Prod]
          }
        }
      }
  }


  def newEmberClient[F[_]: Async](timeout: FiniteDuration = 60.seconds, idleTimeInPool: FiniteDuration = 30.seconds): Resource[F, Client[F]] =
    EmberClientBuilder
      .default[F]
      .withTimeout(timeout)
      .withIdleTimeInPool(idleTimeInPool)
      .build

  implicit val logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  def runClient = {
    //Logger[IO].info(s"Loaded config $clientC")
    newEmberClient[IO]()
      .map { client =>
        EmberClientAtServer(client, UriConfig)
      }
      .use { capp =>
        repeatMonton(capp.getProds, 1, 10L) //, .postProds(ProdName("Sepp"))
      }
      .flatMap { li =>
        IO.println(li)
      }
  }

  override def run: IO[Unit] = runClient

  def repeatMonton[A](ioa: IO[A], delay: Int, nmal: Long) =
    fs2.Stream.fixedRate[IO](delay.seconds).take(nmal).evalMap(_ => ioa).compile.toList

}
