package exa

import cats.effect.IO
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

  val io: IO[String] = IO("Starting a task").debug >> IO.sleep(400.millis) >> IO("Task completed").debug
  val fibExec = for {
    fib <- io.start
    _ <- fib.join
  } yield fib

test("fst"){
  fibExec
}


}
