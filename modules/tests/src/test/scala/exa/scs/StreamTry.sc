import cats.effect.kernel.Deferred
import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO, Sync}
import cats.implicits.{catsSyntaxApplicativeByName, catsSyntaxEitherId, toFunctorOps}
import fs2._
import fs2.concurrent.SignallingRef

import scala.concurrent.duration.DurationInt
import scala.util.Random

def writeToSocket[F[_] : Async](chunk: Chunk[String]): F[Unit] =
  Async[F].async { callback =>
    println(s"[thread: ${Thread.currentThread().getName}] :: Writing $chunk to socket")
    callback(Right(()))
    Sync[F].delay(Some(Sync[F].delay(())))
  }

def writeToSocket0[F[_] : Async](chunk: Chunk[String]): IO[Unit] =
  IO(println(s"[thread: ${Thread.currentThread().getName}] :: Writing $chunk to socket"))


val stw = Stream((1 to 100).map(_.toString): _*)
  .chunkN(10)
  .covary[IO]
  .parEvalMapUnordered(10)(writeToSocket[IO])
  .compile
  .drain

val sti = Stream
  .eval(Deferred[IO, Either[Throwable, Unit]])
  .flatMap { switch =>
    Stream
      .repeatEval(IO(Random.nextInt(5)))
      .metered(1.second)
      .evalTap(IO.println)
    .evalTap { n =>
      switch.complete(().asRight).void.whenA( n == 4)
    }
      .interruptWhen(switch)
      .onFinalize(IO.println("Interrupted!"))
  }.void

sti.compile.toList.unsafeRunSync()

Stream
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



