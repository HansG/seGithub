package shop.http

import cats.effect.{ IO, Ref }
import munit.{ CatsEffectSuite, ScalaCheckEffectSuite }
import cats.implicits._
import cats.effect.implicits._
import org.http4s.Status.Ok
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.syntax._
import org.http4s.client.Client
import skunk.syntax.id

//  https://blog.kubukoz.com/flavors-of-shared-state/
class SharedStateTry extends CatsEffectSuite with ScalaCheckEffectSuite {

  trait Counter {
    def increment: IO[Unit]

    def get: IO[Int]
  }

  def makeCounter(inc: IO[Unit], retrieve: IO[Int]): Counter = new Counter {
    val increment: IO[Unit] = inc
    val get: IO[Int] = retrieve
  }

  val refCounter: IO[Counter] =
    Ref[IO].of(0).map(ref => makeCounter(ref.update(_ + 1), ref.get)).flatTap(c => IO(println("Neuer Counter " + c)))

  val useCounter = for {
    counter <- refCounter
    _ <- counter.increment.parReplicateA(2)
    v <- counter.get
  } yield v

  test("useCounter") {
    useCounter.flatMap(c => IO(println("Wert: " + c)))
  }

  /* §§  http4s... Client ist/hat run : (req: Request[F]) => Resource[F, Response[F]] 
   Test-Client erzeugen: Client.fromHttpApp[IO](httpApp) z.B. HttpApp.pure(Response(Ok).withEntity("HG"))
    aufrufen:  client.run(req):Resource[F, Response[F]] z.B. req=Request() 
    Zugriff auf Antwort: resource.use(resp =>  F[B])
  Server, HttpRoutes[IO] erzeugen: HttpRoutes.of[IO] { case req => IO(Response) }
    aufrufen (ohne WebServer): httpRoutes.orNotFound.run(Request()) : IO[Response]
   */
  def sampleRequest(client: Client[IO]): IO[Unit] = client.run(Request()).use_

  def withCount(client: Client[IO], counter: Counter) = Client[IO] { req =>
    counter.increment.toResource *> client.run(req)
  }
  

  /* §§
   * Response-Antwort t:T  einpacken : Response(Status...).withEntity(t) (using EntityEncoder[F, T]) -> als EntityBody
   * Antwort t:T auspacken response.flatMap(resp => resp.bodyText.compile.string)  siehe @link callRoute
   */
  def routeExClient(client: Client[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case _ =>
      refCounter.flatMap { counter =>
        val countedClient = withCount(client, counter)

        sampleRequest(countedClient) *> sampleRequest(countedClient) *>
          counter.get.map(_.show).map(Response(Ok).withEntity(_))
      }
  }

  def callRouteExClient(route: Client[IO] => IO[HttpRoutes[IO]]): IO[List[String]] = {
    // our fake client, which simply succeeds
    println("Neuer fake client")
    val c = Client.fromHttpApp[IO](HttpApp.pure(Response(Ok).withEntity("HG")))
    callRoute(route(c))
  }

  def callRoute(route: IO[HttpRoutes[IO]]): IO[List[String]] = {
    // our fake client, which simply succeeds
    println("Neuer fake client")
    val c = Client.fromHttpApp[IO](HttpApp.pure(Response(Ok).withEntity("HG")))

    route.flatMap {
      callRoute(_)
    }
  }


    def callRoute(routes: HttpRoutes[IO]): IO[List[String]] = {
      println("Neu: handler run")
      val runIt = routes.orNotFound.run(Request())
      val runAndGetBody = runIt.flatMap(_.bodyText.compile.string).flatTap(s => IO(println("runAndGetBody: " + s)))

      runAndGetBody.replicateA(3)
    }.flatTap {
        case results => IO(println("results: " + results))
      }
      // validate results
      .flatTap {
        case results if results.forall(_ == "2") => IO(println("Success!"))
        case _ => IO(println("Failure!"))
      }

    test("callRouteExClient") {
      callRouteExClient(c => IO(routeExClient(c))).flatTap(
        results =>
          if (results.forall(_ == "2")) IO(println("Success!"))
          else IO(println("Failure!"))
      )
    }

    class UserService(sclient: Client[IO]) {
      println("Neuer UserService")

      private val counter: IO[Counter] = refCounter
      val countedClient = counter.map { counter =>
        (withCount(sclient, counter), counter)
      }

      /*
    Liefert Anzahl der Aufrufe (nicht User)
     */
      def find(id: Int): IO[Option[String]] =
        countedClient.flatMap { countedClient_Count =>
          sampleRequest(countedClient_Count._1).flatMap(_ => countedClient_Count._2.get.map(i => Some(i.show)))
        }
    }

    def routeExService(service: UserService): HttpRoutes[IO] = HttpRoutes.of[IO] {
      //    case POST -> Root / "users" / id =>
      case _ =>
        service.find(1).map {
          case None => println("service.find: "); Response[IO](Ok).withEntity("None")
          case Some(user) => println("service.find: "); Response[IO](Ok).withEntity(user)
        }
      //    case _ => NotFound()
    }

    test("routeExService") {
      callRouteExClient(c => IO(new UserService(c)).map(routeExService(_))).flatTap(
        results =>
          if (results.forall(_ == "2")) IO(println("Success!"))
          else IO(println("Failure!"))
      )
    }

    import cats.effect.IOLocal
    import cats.~>
    import cats.effect.Resource
    import cats.data.Kleisli
    import cats.data.OptionT

    case class CounterWithReset(c: Counter, withFreshCounter: IO ~> IO)

    val localCounterR: IO[CounterWithReset] = IOLocal(0).map { local =>
      val c = makeCounter(
        local.update(_ + 1),
        local.get
      )
      /* §§ Trick  gegeben ein Effekt run:F[A] ->  einen Effekt beforeRun unmittelbar VOR  run und 
      einen Effekt afterRun unmittelbar NACH run (d.h. nach "Beendigung" von run) ausführen (im selben Fiber): run => Resource.make(beforRun)(_ => afterRun).surroundK(run): 
    -> hier local.reset unmittelbar nach Erzeugung des Response
     */
      CounterWithReset(c, Resource.make(IO.unit)(_ => local.reset).surroundK)
    }

    def withCountReset(r: HttpRoutes[IO], c: CounterWithReset): HttpRoutes[IO] = Kleisli { req =>
      OptionT {
        c.withFreshCounter(r.run(req).value)
      }
    }

    def withCountReset1(r: HttpRoutes[IO], c: CounterWithReset): HttpRoutes[IO] =
      r.mapF(_.mapK(c.withFreshCounter))




  def routeExClientLocal(rawClient: Client[IO]): IO[HttpRoutes[IO]] =
      localCounterR.map { counterWithReset =>
        val counter = counterWithReset.c

        val client = withCount(rawClient, counter)

        val r = routeExClientLocalBase(client, counter)

        withCountReset(
          r,
          counterWithReset
        )
      }

    def routeExClientLocalBase(client: Client[IO], c: Counter): HttpRoutes[IO] = HttpRoutes.of[IO] {
      case _ =>
        sampleRequest(client) *>
          sampleRequest(client) *>
          c.get.map(_.show).map(Response().withEntity(_))
    }

  def routeExClientLocalBase1(client: Client[IO], c: Counter): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case _ =>
      (
        sampleRequest(client) &>
          sampleRequest(client)
        ) *> c.get.map(_.show).map(Response().withEntity(_))
  }


    val ioRoute = IOLocal(0).map { local =>
      val c = makeCounter(local.update(_ + 1), local.get)
      val cc = withCount(Client.fromHttpApp[IO](HttpApp.pure(Response(Ok).withEntity("HG"))), c)
      val r = routeExClientLocalBase(cc, c)
      r.mapF(_.mapK(Resource.make(IO.unit)(_ => local.reset).surroundK))
    }

    test("routeExClientLocal1") {
      ioRoute.flatMap {
        callRoute(_)
      }.flatTap(
        results =>
          if (results.forall(_ == "2")) IO(println("Success!"))
          else IO(println("Failure!"))
      )
    }

    test("routeExClientLocal") {
      callRouteExClient(routeExClientLocal).flatTap(
        results =>
          if (results.forall(_ == "2")) IO(println("Success!"))
          else IO(println("Failure!"))
      )
    }

}
