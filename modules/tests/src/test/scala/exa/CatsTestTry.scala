package exa

import cats.effect.IO
import cats.effect.kernel.Outcome
import munit.CatsEffectSuite

import scala.concurrent.duration._
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
    _ <- fib.join
  } yield fib
  test("fibExec"){
    fibExec
  }


  val fibCancel = for {
    fib <- io.start
    _ <- IO.sleep(100.millis) >> fib.cancel >> IO("Fiber cancelled").debug
    res <- fib.join
  } yield res
  test("fibCancel") {
    outcome(fibCancel)
    //   fibCancel
  }


  def outcome(res: IO[Outcome[IO, Throwable, String]]): IO[String] = res flatMap {
    case Outcome.Succeeded(fa) => IO("fiber executed successfully").debug
    case Outcome.Errored(e) => IO("error occurred during fiber execution").debug
    case Outcome.Canceled() => IO("fiber was canceled!").debug
  }


}
