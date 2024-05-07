package shop.http

import cats.effect.{IO, Ref}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import cats.implicits._
import cats.effect.implicits._
import cats.mtl.syntax.local
import cats.syntax.flatMap
import org.checkerframework.checker.units.qual.m
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
    _ <- counter.increment.parReplicateA(2) //§§ parReplicateA: parallel wiederholen!
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
    Test-aufrufen (ohne WebServer): httpRoutes.orNotFound.run(Request()) : IO[Response]
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

  def callAppExClient(route: Client[IO] => IO[HttpRoutes[IO]]): IO[List[String]] = {
    // our fake client, which simply succeeds
    println("Neuer fake client")
    val c = Client.fromHttpApp[IO](HttpApp.pure(Response(Ok).withEntity("HG")))
    callApp(route(c))
  }

  def callApp(route: IO[HttpRoutes[IO]]): IO[List[String]] = {
    // our fake client, which simply succeeds
    println("Neuer fake client")
    val c = Client.fromHttpApp[IO](HttpApp.pure(Response(Ok).withEntity("HG")))

    route.flatMap {
      callApp(_)
    }
  }


  test("callRouteExClient") {
    callAppExClient(c => IO(routeExClient(c))).flatTap(
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
    callAppExClient(c => IO(new UserService(c)).map(routeExService(_))).flatTap(
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

  case class CounterWithReset(counter: Counter, withFreshCounter: IO ~> IO)


  def withCountReset(r: HttpApp[IO], c: CounterWithReset): HttpApp[IO] = Kleisli { req =>
    OptionT {
      c.withFreshCounter(r.run(req))
    }
  }

  def withCountReset1(r: HttpRoutes[IO], c: CounterWithReset): HttpRoutes[IO] =
    r.mapF(_.mapK(c.withFreshCounter))

  case class Mode()

  object Old extends Mode

  object New extends Mode
/*
§§ Plan AKDB
mkCounter1(Local)=(ref <- local.get; mkCounter(ref))
    clientWithCount(fakeClient, Counter) = ....
   withFreshRef: run =>  Resource[Client](pre: newRef->local)(post:local.reset).use(run)
   Local , fakeClient -> clientWithCount = clientWithCount(fakeClient, mkCounter1(Local))
                      ->  HttRoutes/App :
                              withFreshRef:
                              call(clientWithCount).replicate in versch. Fibers
 */

  def localRefCounter(implicit m: Mode): IO[CounterWithReset] = m match {
    case Old => IOLocal(0).map { local =>
      val c = makeCounter(
        local.update(_ + 1),
        local.get
      )
      /* §§ Trick: "Lebenszyklus" von Resource nutzen, um ein gegebenes IO (run) mappen auf ein IO (d.h. IO ~> IO , allg. F ~> F)  mit:  unmittelbar vor und/oder nach run wird ein IO (beforeRun) und ein  IO (afterRun) ausgeführt:  run => Resource.make(beforeRun)(_ => afterRun).surroundK(run) (==.use(_ => run):
        Folgende Aktionen werden per Lebenszyklus unmittelbar nacheinander ausgeführt (im selben Fiber):
        1.Aktion  beforeRun (Lebenszyklus:erzeuge Resource)  2.Aktion: run (Lebenszyklus:benutze Resource, hier use(_ => run), d.h. nur formal) 3.Aktion  afterRun unmittelbar NACH run (d.h. nach "Beendigung" von run)
        (hier nur local.reset unmittelbar nach run, vgl. unten auchmit beforeRun)

        §§ Trick: eine Resource[IO,A] mappen auf eine Resource[IO,A] mit: a:A bleibt dasselbe, jedoch vor und/oder nach run in Resource[IO,A].use( run(_)) wird ein anderes IO ausgeführt! (vgl. withCount)
       */
      CounterWithReset(c, Resource.make(IO.unit)(_ => local.reset).surroundK)
    }

    case New =>
      // 1
      Ref[IO].of(0).flatMap(IOLocal(_)).map { local =>
        // 2
        val c = makeCounter(
          local.get.flatMap(_.update(_ + 1)),
          local.get.flatMap(_.get)
        )

        // 3
        val withFreshK = Resource.make(Ref[IO].of(0).flatMap(local.set))(_ => local.reset).surroundK

        // 4
        CounterWithReset(c, withFreshK)
      }
  }

  /*
  §§ Resource : NonEmptyParallel -> parMapN: Resourcen parallel erzeugen und mappen
  andere Mgl. ist flatMap:
  def mkResource(s: String) = Resource.make(IO(println(s"Acquiring $$s")) *> IO. pure(s))(IO(println(s"Releasing $$_")))
    val r = for {   outer <- mkResource("outer") ;   inner <- mkResource("inner") } yield (outer, inner)
    r. use { case (a, b) =>   IO(println(s"Using $$a and $$b")) }
   */


  def routeExClientLocal(rawClient: Client[IO]): IO[HttpRoutes[IO]] =
    localRefCounter.map { counterWithReset =>
      val counter = counterWithReset.counter

      val client = withCount(rawClient, counter)

      val r = routeExClient(client, counter)

      withCountReset(
        r,
        counterWithReset
      )
    }


  def routeExClient(client: Client[IO], c: Counter)(implicit m: Mode): HttpRoutes[IO]  = m match {
    case Old =>
      HttpRoutes.of[IO] {
        case _ =>
          sampleRequest(client) *>
            sampleRequest(client) *>
            c.get.map(_.show).map(Response().withEntity(_))
      }
    case _ =>
      HttpRoutes.of[IO] {
        case _ =>
          sampleRequest(client) &>
            sampleRequest(client) *>
              c.get.map(_.show).map(Response().withEntity(_))
      }
  }

  def appExClient(client: Client[IO], c: Counter)(implicit m: Mode): HttpApp[IO] = routeExClient.orNotFound


  def callApp(routes: HttpRoutes[IO]): IO[List[String]] = {
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


  def ioRoute =
    for {
      cwr <- localRefCounter
            cc = withCount(Client.fromHttpApp[IO](HttpApp.pure(Response(Ok).withEntity("HG"))), cwr.counter)
            r = routeExClient(cc, cwr.counter)
      r1 <- cwr.withFreshCounter(IO(r))
    }  yield r1

  def ioRoute1 =
    for {
      local <- Ref[IO].of(0).flatMap(IOLocal(_))
            counter = makeCounter(
              local.get.flatMap(_.update(_ + 1)),
              local.get.flatMap(_.get)
            )
            cc = withCount(Client.fromHttpApp[IO](HttpApp.pure(Response(Ok).withEntity("HG"))), counter)
            r = routeExClient(cc, counter)
      r1 <- Resource.make(Ref[IO].of(0).flatMap(local.set))(_ => local.reset).use(_ => IO(r))
    }  yield r1


  def ioRoute2 =
    for {
      local <- Ref[IO].of(0).flatMap(IOLocal(_))
    }  yield {
      val counter = makeCounter(
        local.get.flatMap(_.update(_ + 1)),
        local.get.flatMap(_.get)
      )
      val cc = withCount(Client.fromHttpApp[IO](HttpApp.pure(Response(Ok).withEntity("HG"))), counter)
      (routeExClient(cc, counter), local)
    }

  def ioRoute3 =
    for {
      (route, local) <- ioRoute2
      resultL <- Resource.make(Ref[IO].of(0).flatMap(local.set))(_ => local.reset).use(_ => callApp(route))
    }  yield {
      resultL
    }



  implicit val mode : Mode = Old


  test("routeExClientLocal3") {
    ioRoute
      .flatMap {
        callApp(_)
      }
      .flatTap(
        results =>
          if (results.forall(_ == "2")) IO(println("Success!"))
          else IO(println("Failure!"))
      )
  }



  test("routeExClientLocal1") {
    ioRoute
      .flatMap {
        callApp(_)
      }
      .flatTap(
        results =>
          if (results.forall(_ == "2")) IO(println("Success!"))
          else IO(println("Failure!"))
      )
  }

  test("routeExClientLocal") {
    callAppExClient((rawClient: Client[IO]) => routeExClientLocal(rawClient)).flatTap(
      results =>
        if (results.forall(_ == "2")) IO(println("Success!"))
        else IO(println("Failure!"))
    )
  }

}
