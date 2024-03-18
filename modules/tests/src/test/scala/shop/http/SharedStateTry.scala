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

  val refCounter: IO[Counter] = Ref[IO].of(0).map(ref => makeCounter(ref.update(_ + 1), ref.get))

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
  Antwort einpacken t als EntityBody: Response(Status...).withEntity(t) (using EntityEncoder[F, T])
  siehe @link testRoute
   */
  def routes(client: Client[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
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
  def testRoute(route: Client[IO] => IO[HttpRoutes[IO]]): IO[List[String]] = {
    // our fake client, which simply succeeds
    val c = Client.fromHttpApp[IO](HttpApp.pure(Response()))

    // build the route: the "IO" part will be useful later
    route(c).flatMap { handler =>
      // run the route with a simple request and grab its body
      val httpApp = handler
        .orNotFound
      val runAndGetBody = httpApp.run(Request()).flatMap(resp => resp.bodyText.compile.string)

      // Run the request 10 times in parallel
      runAndGetBody
        .parReplicateA(10)
    }
  }
  // validate results
    .flatTap {
      case results => IO(println("results: " + results))
    }
    // validate results
    .flatTap {
      case results if results.forall(_ == "2") => IO(println("Success!"))
      case _                                   => IO(println("Failure!"))
    }

  test("testRoute") {
    testRoute(client => IO.pure(routes(client)))
  }

}
