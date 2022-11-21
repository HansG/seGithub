package exa.scs

import cats.effect.kernel.Deferred
import cats.effect.std.Supervisor
import cats.effect.testkit.TestControl
import cats.effect.{Async, IO, Sync}
import cats.implicits.catsSyntaxEitherId
import fs2._
import fs2.concurrent.SignallingRef
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}

import scala.concurrent.duration.DurationInt
import scala.util.Random


/*
Besser IOApp als Worksheet: nur ausgewähltes läuft, kein Rebuild Projekt nötig, damit imports in Worksheet funktionieren
 */
//object StreamTry extends IOApp {
class StreamTry extends CatsEffectSuite with ScalaCheckEffectSuite {

  def writeToSocket[F[_]: Async](chunk: Chunk[String]): F[Unit] =
    Async[F].async { callback =>
      println(s"[thread: ${Thread.currentThread().getName}] :: Writing $chunk to socket")
      callback(Right(()))
      Sync[F].delay(Some(Sync[F].delay(())))
    }

  def writeToSocket0[F[_]: Async](chunk: Chunk[String]): IO[Unit] =
    IO(println(s"[thread: ${Thread.currentThread().getName}] :: Writing $chunk to socket"))

  val stpwrite = Stream((1 to 100).map(_.toString): _*)
    .chunkN(10)
    .covary[IO]
    .parEvalMapUnordered(10)(writeToSocket[IO])

  test("run  Stream parEvalMapUnordered  chunkN ") {
    runStream(stpwrite)
  }



  val stinterrupt = Stream
    .eval(Deferred[IO, Either[Throwable, Unit]])
    .flatMap { switch =>
      Stream
        .repeatEval(IO(Random.nextInt(5)))
        .metered(1.second)
        .evalTap(IO.println)
        .evalTap { n =>
          switch.complete(().asRight).void.whenA(n == 4)
        }
        .interruptWhen(switch)
        .onFinalize(IO.println("Interrupted!"))
    }


  test("run  Stream interruptWhen  Deferred complete  ") {
    runStream(stinterrupt)
  }


  val stpause =  Stream
    .eval(SignallingRef[IO, Boolean](false))
    .flatMap { signal =>
      val src =
        Stream
          .repeatEval(IO.println("ping"))
          .pauseWhen(signal)
          .metered(1.second)
      val pause =
        Stream
          .sleep[IO](3.seconds)
          .evalTap(_ => IO.println("ႍ>> Pausing stream ႍ<<"))
          .evalTap(_ => signal.set(true))
      val resume =
        Stream
          .sleep[IO](7.seconds)
          .evalTap(_ => IO.println("ႍ>> Resuming stream ႍ<<"))
          .evalTap(_ => signal.set(false))
      Stream(src, pause, resume).parJoinUnbounded
    }
    .interruptAfter(10.seconds)
    .onFinalize(IO.println("pong"))

  test("run  Stream pauseWhen SignallingRef  ") {
    runStream(stpause)
  }


  val data: Stream[IO,Int] = {
    Stream.range(1, 10).covary[IO].metered(1.seconds)
  }
  val stSig = Stream.eval(fs2.concurrent.SignallingRef[IO,Int](0)).flatMap(s =>
    Stream(s).concurrently(data.evalMap(s.set))).flatMap(_.discrete).debug().takeWhile(_ < 7, true)
  // .compile.last.unsafeRunSync()

  test("run SignallingRef discrete stream") {
    runStream(stSig)
  }


  val parEvalSt = Stream(1,2,3,4,6,7,8,9).covary[IO].parEvalMapUnordered(2)(i => IO.sleep(Random.nextInt(2).seconds) *> IO(println(i)))
  test("run parEvalMapUnordered") {
    runStream0(parEvalSt)
  }

  test("run parEvalMapUnordered") {
    supervised(parEvalSt.compile.drain)
  }


  def runStream0(stream : Stream[IO,_] ) =
    stream.compile.drain .map { r =>  assertEquals(true, true) }

  def runStream(stream : Stream[IO,_] ) =
    TestControl.executeEmbed(stream.compile.drain).map { r =>
      assertEquals(true, true)
    }

  def supervised[A](fa : IO[A] ) =
    Supervisor[IO].use {   sp =>
          sp.supervise(fa).void
    }



}
