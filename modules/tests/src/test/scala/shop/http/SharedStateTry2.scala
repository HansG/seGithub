package shop.http

import cats.effect.{IO, Ref}
import cats.implicits.catsSyntaxParallelAp1
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}

class SharedStateTry2  extends CatsEffectSuite with ScalaCheckEffectSuite {

  trait Counter {
    def increment: IO[Unit]
    def get: IO[Int]
  }

  def makeCounter(inc: IO[Unit], retrieve: IO[Int]): Counter = new Counter {
    val increment: IO[Unit] = inc
    val get: IO[Int] = retrieve
  }


  val refCounter: IO[Counter] =
    Ref[IO].of(0).map { ref =>
       makeCounter(ref.update(_ + 1), ref.get)
    }.flatTap(c => IO(println("Neuer Counter " + c)))

  val refCounterd: IO[Counter] =
    Ref[IO].of(0).map { ref =>
      new Counter {
        def increment: IO[Unit] = ref.update(_ + 1)
        def get: IO[Int] = ref.get
      }
    }.flatTap(c => IO(println("Neuer Counter " + c)))


  val useCounter = for {
    counter <- refCounter
    _ <- counter.increment.parReplicateA(2) //§§ parReplicateA: parallel wiederholen!
    v <- counter.get
  } yield v

  test("useCounter") {
    useCounter.flatMap(c => IO(println("Wert: " + c)))
  }












}
