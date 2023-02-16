package exa

import cats.effect.{ Deferred, IO }
import cats.effect.kernel.Outcome
import cats.implicits.toTraverseOps
import munit.CatsEffectSuite

import scala.concurrent.duration._
import scala.util.Random
/*
https://www.baeldung.com/scala/cats-effect-fibers-concurrent-programming
https://medium.com/@PerrottaFrancisco/learning-cats-effects-concurrency-pt-2-bd9a725ad932
 */
class CatsTestTry extends CatsEffectSuite {

  implicit class Xtensions[A](io: IO[A]) {
    def debug: IO[A] =
      for {
        a <- io
        _ = println(s"[${Thread.currentThread().getName}] " + a)
      } yield a
  }

  val io: IO[String] = IO("Starting a task").debug() >> IO.sleep(1000.millis) >> IO("Task completed").debug()

  def runIOinIO[A](io: IO[A]) =
    for {
      fib <- io.start
      out <- fib.join //!!! sonst wird (2.) println nicht ausgegeben (weil anderer Thread!?), mit join werden Ausgaben(?) des anderen Threads geholt ...nicht jedoch in runOut/test("racePair")
    } yield out

  test("fibExec") {
    debugIOOut(runIOinIO(io)) // Outcome enthält NUR letztes FlatMap !!!?? (hier IO("Task completed")
  }

  val ioWithCancelationHook = io.onCancel(IO("Applying cancelation finalizer").debug.void)

  def fibCancel(io: IO[String]) =
    for {
      fib <- io.start
      _   <- IO.sleep(100.millis) >> fib.cancel >> IO("Fiber cancelled").debug
      out <- fib.join
    } yield out

  test("fibCancel") {
    // debugIOOut(fibCancel(io))
    debugIOOut(fibCancel(ioWithCancelationHook))
  }

  def unpackOut(out: Outcome[IO, Throwable, String]): Either[Throwable, Option[IO[String]]] =
    (out match {
      case Outcome.Succeeded(fa) => Right(Some(fa))
      case Outcome.Errored(e)    => Left(e)
      case Outcome.Canceled()    => Right(None)
    })

  def runOut(out: Outcome[IO, Throwable, String]): IO[Unit] = {
     unpackOut(out).map(opt => runIOinIO(opt.get.debug()).debug().as(())).getOrElse(IO(())) //io in opt liefert keine Console-Ausgabe !!?? läuft trotzdem???
  }

  def debugOut(out: Outcome[IO, Throwable, String]): IO[String] =
    (out match {
      case Outcome.Succeeded(fa) => IO(s"Fiber-Outcome: success-result: $fa")
      case Outcome.Errored(e)    => IO(s"Fiber-Outcome: error: ${e}")
      case Outcome.Canceled()    => IO("Fiber-Outcome: canceled")
    }).debug()

  def debugIOOut(res: IO[Outcome[IO, Throwable, String]]): IO[String] = res flatMap { debugOut(_) }

  val participant1 = IO("Start Task1").debug() >> IO.sleep(Random.nextInt(1000).millis) >> IO("Task 1 completed").debug()
  val participant2 = IO("Start Task2").debug() >> IO.sleep(Random.nextInt(1000).millis) >> IO("Task 2 completed").debug()

  test("race") {
    IO.race(
        participant1.onCancel(IO("part..1 cancelled").debug().void),
        participant2.onCancel(IO("part..2 cancelled").debug().void)
      )
      .debug()
  }

  test("racePair") {
    IO.racePair(
        participant1.onCancel(IO("part..1 cancelled").debug().void),
        participant2.onCancel(IO("part..2 cancelled").debug().void)
      )
      .flatMap { either =>
        either.fold(
          o1f2 => debugOut(o1f2._1) >> runOut(o1f2._1) >> o1f2._2.cancel,//
          f1o2 => f1o2._1.cancel >> debugOut(f1o2._2).void >> runOut(f1o2._2) //
        )
      }
  }

  //@PerrottaFrancisco
  test("100 fibers concurrently - main fiber joins on each") {
    for {
      state  <- IO.ref(0)
      fibers <- state.update(_ + 1).start.replicateA(100)
      _      <- fibers.traverse(_.join).void
      value  <- state.get
      _      <- IO.println(s"The final value is: $value")
    } yield ()
  }


  //@PerrottaFrancisco
  test("main fiber spawns a fiber that counts down. When countdown=5 -> waits on a Deferred (..completed 5 seconds later by main fiber) main fiber  waits for countdown") {
    def countdown(n: Int, pause: Int, waiter: Deferred[IO, Unit]): IO[Unit] =
      IO.println(n) *> IO.defer {
        if (n == 0) IO.unit
        else if (n == pause) IO.println("paused...") *> waiter.get *> countdown(n - 1, pause, waiter)
        else countdown(n - 1, pause, waiter)
      }

    for {
      waiter <- IO.deferred[Unit]
      f <- countdown(10, 5, waiter).start
      _ <- IO.sleep(5.seconds)
      _ <- waiter.complete(())
      _ <- f.join
      _ <- IO.println("blast off!")
    } yield ()
  }




  def ioWithTimeout(io : IO[String]): IO[String] = io.timeout(400.millis)
  def withFallback(io : IO[String]) = io.timeoutTo(400.millis, IO("Fallback IO executed after timeout").debug())

  test("timeout") {
    ioWithTimeout(io)
  }

}
