import cats.effect.kernel.Deferred
import cats.effect.{Async, ExitCode, IO, IOApp, Sync}
import cats.implicits.{catsSyntaxApplicativeByName, catsSyntaxEitherId, toFunctorOps}
import fs2._
import fs2.concurrent.SignallingRef

import scala.concurrent.duration.DurationInt
import scala.util.Random
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import cats.effect.testkit.TestControl


/*
Besser IOApp als Worksheet: nur ausgewähltes läuft, kein Rebuild Projekt nötig, damit imports in Worksheet funktionieren

nur ausgewähltes läuft: Probieren!! ->
"
package upperbound

import scala.concurrent.ExecutionContext
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}

abstract class BaseSuite extends CatsEffectSuite with ScalaCheckEffectSuite {
  override val munitExecutionContext: ExecutionContext = ExecutionContext.global
}

class LimiterSuite extends BaseSuite {
..
  test("submit semantics should return the result of the submitted job") {
    IO.ref(false)
      .flatMap { complete =>
        ..
      }
      .map { case (res, state) =>
        assertEquals(res, "done")
        assertEquals(state, true)
      }
  }
  ....
}
"
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
    run(stpwrite.compile.drain)
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
    .void

  test("run  Stream interruptWhen  Deferred complete  ") {
    run(stinterrupt.compile.drain)
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
    run(stpause.compile.drain)
  }


  val data: Stream[IO,Int] = {
     Stream.range(1, 10).covary[IO].metered(1.seconds)
   }
   val stSig = Stream.eval(fs2.concurrent.SignallingRef[IO,Int](0)).flatMap(s =>
     Stream(s).concurrently(data.evalMap(s.set))).flatMap(_.discrete).debug().takeWhile(_ < 7, true)
    // .compile.last.unsafeRunSync()

  test("run SignallingRef discrete stream") {
    run(stSig.compile.drain)
  }




  def run(prog : IO[_]) =
    TestControl.executeEmbed(prog).map { r =>
      assertEquals(true, true)
    }



}
