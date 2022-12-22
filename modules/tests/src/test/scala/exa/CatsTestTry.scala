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

  val runIOinIO = for {
    fib <- io.start
    out <- fib.join  //!!! sonst wird (2.) println nicht ausgegeben (weil anderer Thread), mit join werden Ausgaben(?) des anderen Threads geholt
  } yield out
  test("fibExec"){
    runIOinIO
  }

  val ioWithCancelationHook = io.onCancel(IO("Applying cancelation finalizer").debug.void)

  def fibCancel(io : IO[String]) = for {
    fib <- io.start
    _ <- IO.sleep(100.millis) >> fib.cancel >> IO("Fiber cancelled").debug
    out <- fib.join
  } yield out

  test("fibCancel") {
    debugIOO(fibCancel(io))
    //   outcome(fibCancel(ioWithCancelationHook))
    //   fibCancel
  }



  def unpackO(out: Outcome[IO, Throwable, String]):Either[Throwable, Option[IO[String]]] =  (out match {
    case Outcome.Succeeded(fa) => Right(Some(fa))
    case Outcome.Errored(e) => Left(e)
    case Outcome.Canceled() => Right(None)
  })


  def debugO(out: Outcome[IO, Throwable, String]): IO[String] =  (out match {
    case Outcome.Succeeded(fa) => IO(s"Fiber-Outcome: success-result: $fa")
    case Outcome.Errored(e) => IO(s"Fiber-Outcome: error: ${e}")
    case Outcome.Canceled() => IO("Fiber-Outcome: canceled")
  }).debug

  def debugIOO(res: IO[Outcome[IO, Throwable, String]]): IO[String] =  res flatMap { debugO(_)  }


  val participant1 = IO("Start Task1").debug >> IO.sleep(Random.nextInt(1000).millis) >> IO("Task 1 completed").debug
  val participant2 = IO("Start Task2").debug >> IO.sleep(Random.nextInt(1000).millis) >> IO("Task 2 completed").debug

  test("race") {
    IO.race(participant1.onCancel(IO("part..1 cancelled").debug.void), participant2.onCancel(IO("part..2 cancelled").debug.void)).debug
  }
  test("racePair") {
    IO.racePair(participant1.onCancel(IO("part..1 cancelled").debug.void), participant2.onCancel(IO("part..2 cancelled").debug.void)).
      flatMap { either    =>
        either.fold ( o1f2 => debugO(o1f2._1) >>   unpackO(o1f2._1).forall( runIOinIO( oio.get))  >> o1f2._2.cancel , f1o2 => f1o2._1.cancel >> debugO(f1o2._2).void  >> runIOinIO(f1o2._2)  )
      }
  }

}
