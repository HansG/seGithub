package shop.services

import cats.effect.{IO, Ref}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import cats.implicits._
import cats.effect.implicits._
import org.http4s.Status.Ok
import org.http4s._, org.http4s.dsl.io._, org.http4s.implicits._
import org.http4s.syntax._
import org.http4s.client.Client


class SharedStateTry  extends CatsEffectSuite with ScalaCheckEffectSuite {

  trait Counter {
    def increment: IO[Unit]
    def get: IO[Int]
  }

  def makeCounter(inc: IO[Unit], retrieve: IO[Int]): Counter = new Counter{
    val increment: IO[Unit] = inc
    val get: IO[Int] = retrieve
  }

  val refCounter: IO[Counter] = Ref[IO].of(0).map { ref =>
    makeCounter(
      ref.update(_ + 1),
      ref.get
    )
  }

  val useCounter = for {
    counter <- refCounter
    _       <- counter.increment.parReplicateA(2)
    v       <- counter.get
  } yield v


  test("useCounter"){
    println(useCounter.unsafeRunSync())
  }

  def sampleRequest(client: Client[IO]): IO[Unit] = client.run(Request()).use_

  def withCount(client: Client[IO], counter: Counter): Client[IO] = Client { req =>
    counter.increment.toResource *>
      client.run(req)
  }

  def routes(client: Client[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case _ =>
      refCounter.flatMap { counter =>
        val countedClient = withCount(client, counter)

        sampleRequest(countedClient) *> sampleRequest(countedClient) *>
          counter.get.map(_.show).map(Response(Ok).withEntity(_))
      }
  }

  def testRoute(route: Client[IO] => IO[HttpRoutes[IO]]): IO[List[String]] = {
    // our fake client, which simply succeeds
    val c = Client.fromHttpApp[IO](HttpApp.pure(Response()))

    route(c).flatMap { handler =>
      val runAndGetBody = handler.orNotFound.run(Request())
      
      
      
      
    }



  }



}
