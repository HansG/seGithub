package shop.http

import cats.effect.{IO, Ref}
import cats.implicits._
import cats.effect.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.http4s._
import org.http4s.client.Client


//https://blog.kubukoz.com/flavors-of-shared-state/
class SharedStateTry extends CatsEffectSuite with ScalaCheckEffectSuite {

  trait Counter {
    def increment: IO[Unit]
    def get: IO[Int]
  }

  def makeCounter(inc: IO[Unit], retrieve: IO[Int]): Counter = new Counter{
    val increment: IO[Unit] = inc
    val get: IO[Int] = retrieve
  }

  val refCounter: IO[Counter] = Ref[IO].of(0).map(ref =>
    makeCounter(ref.update(_ + 1), ref.get)
  )

  val useCounter = for {
    counter <- refCounter
    v <- counter.increment.parReplicateA(2)
  } yield v


  test("useCounter"){
    useCounter.unsafeRunSync()

  }



  def sampleRequest(client: Client[IO]): IO[Unit] = client.run(Request()).use_

  def withCount(client : Client[IO], counter : Counter) = Client[IO] {req =>
       counter.increment.toResource  *> client.run(req)
  }

  def routes(client: Client[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case _ =>
      val sr = refCounter.flatMap( counter =>
        sampleRequest(withCount(client, counter))  )
      sr.

      )


  }



}
