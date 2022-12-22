package exa

import cats.effect.IO
import cats.effect.kernel.Outcome
import munit.CatsEffectSuite

import scala.concurrent.duration._
import scala.util.Random
/*
https://www.baeldung.com/scala/cats-effect-fibers-concurrent-programming
 */
class CatsTestTry extends CatsEffectSuite {

  implicit class Xtensions[A](io: IO[A]) {
    def debug: IO[A] =
      for {
        a <- io
        _ = println(s"[${Thread.currentThread().getName}] " + a)
      } yield a
  }

  val io: IO[String] = IO("Starting a task").debug >> IO.sleep(1000.millis) >> IO("Task completed").debug

  val fibExec = for {
    fib <- io.start
    _ <- fib.join  //!!! sonst wird (2.) println nicht ausgegeben (weil anderer Thread), mit join werden Ausgaben(?) des anderen Threads geholt
  } yield fib
  test("fibExec"){
    fibExec
  }

  val ioWithCancelationHook = io.onCancel(IO("Applying cancelation finalizer").debug.void)

  def fibCancel(io : IO[String]) = for {
    fib <- io.start
    _ <- IO.sleep(100.millis) >> fib.cancel >> IO("Fiber cancelled").debug
    res <- fib.join
  } yield res

  test("fibCancel") {
    ioOutcome(fibCancel(io))
    //   outcome(fibCancel(ioWithCancelationHook))
    //   fibCancel
  }



  def outcome(res: Outcome[IO, Throwable, String]): IO[String] =  res match {
    case Outcome.Succeeded(fa) => IO(s"Fiber-Outcome: success-result: $fa")
    case Outcome.Errored(e) => IO(s"Fiber-Outcome: error: ${e}")
    case Outcome.Canceled() => IO("Fiber-Outcome: canceled")
  }

  def ioOutcome(res: IO[Outcome[IO, Throwable, String]]): IO[String] = {
    val res1 = res flatMap { outcome(_) }
    res1.debug
  }


  val participant1 = IO("Start Task1").debug >> IO.sleep(Random.nextInt(1000).millis) >> IO("Task 1 completed").debug
  val participant2 = IO("Start Task2").debug >> IO.sleep(Random.nextInt(1000).millis) >> IO("Task 2 completed").debug

  test("race") {
    IO.racePair(participant1.onCancel(IO("part..1 cancelled").debug.void), participant2.onCancel(IO("part..2 cancelled").debug.void)).
      flatMap { either    =>
        either.fold ( o1f2 => outcome(o1f2._1) >> o1f2._2.cancel ,  f1o2 => f1o2._1.cancel >> outcome(f1o2._2).void  )
      }
  }

}
