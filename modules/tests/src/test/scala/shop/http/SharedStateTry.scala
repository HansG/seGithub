package shop.http

import cats.effect.{ IO, Ref }
import munit.{ CatsEffectSuite, ScalaCheckEffectSuite }
import cats.implicits._
import cats.effect.implicits._
import org.http4s.Status.Ok
import org.http4s._, org.http4s.dsl.io._, org.http4s.implicits._
import org.http4s.syntax._
import org.http4s.client.Client

//https://blog.kubukoz.com/flavors-of-shared-state/
class SharedStateTry extends CatsEffectSuite with ScalaCheckEffectSuite {

  trait Counter {
    def increment: IO[Unit]
    def get: IO[Int]
  }

  def makeCounter(inc: IO[Unit], retrieve: IO[Int]): Counter = new Counter {
    val increment: IO[Unit] = inc
    val get: IO[Int]        = retrieve
  }

  val refCounter: IO[Counter] =
    Ref[IO].of(0).map(ref => makeCounter(ref.update(_ + 1), ref.get)).flatTap(c => IO(println("Neuer Counter " + c)))

  val useCounter = for {
    counter <- refCounter
    _       <- counter.increment.parReplicateA(2)
    v       <- counter.get
  } yield v

  test("useCounter") {
    println(useCounter.unsafeRunSync())
  }

  def sampleRequest(client: Client[IO]): IO[Unit] = client.run(Request()).use_

  def withCount(client: Client[IO], counter: Counter) = Client[IO] { req =>
    counter.increment.toResource *> client.run(req)
  }

  /** §§
  Antwort t  einpacken als EntityBody: Response(Status...).withEntity(t) (using EntityEncoder[F, T])
   auspacken siehe @link testRoute
    */
  def routeToServerClient(client: Client[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case _ =>
      refCounter.flatMap { counter =>
        val countedClient = withCount(client, counter)

        sampleRequest(countedClient) *> sampleRequest(countedClient) *>
          counter.get.map(_.show).map(Response(Ok).withEntity(_))
      }
  }

  /** §§
    * HttpRoutes[IO] testen ohne Server: httpRoutes.orNotFound.run(Request())
    * Antwort auspacken:  Response[IO].flatMap(resp => resp.bodyText.compile.string)
    */
  def callRouteToServerClient(route: Client[IO] => IO[HttpRoutes[IO]]): IO[List[String]] = {
    // our fake client, which simply succeeds
    println("Neuer fake client")
    val c = Client.fromHttpApp[IO](HttpApp.pure(Response(Ok).withEntity("HG")))

    route(c).flatMap { handler =>
      val runIt         = handler.orNotFound.run(Request())
      val runAndGetBody = runIt.flatMap(_.bodyText.compile.string).flatTap(s => IO(println(s)))

      runAndGetBody.replicateA(3)
    }
  }.flatTap {
      case results => IO(println("results: " + results))
    }
    // validate results
    .flatTap {
      case results if results.forall(_ == "2") => IO(println("Success!"))
      case _                                   => IO(println("Failure!"))
    }

  test("clientCall") {
    callRouteToServerClient(c => IO(routeToServerClient(c))).flatTap(
      results =>
        if (results.forall(_ == "2")) IO(println("Success!"))
        else IO(println("Failure!"))
    )
  }

  class UserService(sclient: Client[IO]) {
    println("Neuer UserService")

    private val counter: IO[Counter] = refCounter
    val countedClient = counter.map { counter =>
      withCount(sclient, counter)
    }

    /*
    Liefert Anzahl der Aufrufe (nicht User)
     */
    def find(id: Int): IO[Option[String]] =
      countedClient.flatMap { countedClient =>
        sampleRequest(countedClient).flatMap(_ => counter.flatMap(c => c.get.map(i => Some(i.show))))
      }
  }

  def routeToService(service: UserService): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case POST -> Root / "users" / id =>
      service.find(id.toInt).map {
        case None       => Response[IO](Ok).withEntity("None")
        case Some(user) => Response[IO](Ok).withEntity(user)
      }
    case _ => NotFound()
  }

  test("routeToService") {
    callRouteToServerClient(c => IO(new UserService(c)).map(routeToService(_))).flatTap(
      results =>
        if (results.forall(_ == "2")) IO(println("Success!"))
        else IO(println("Failure!"))
    )
  }

}
