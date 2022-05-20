package shop

import cats.NonEmptyParallel
import cats.effect._
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import fs2._

import scala.util.Random

object NonEmptyParalell extends IOApp {

  def randomMessageF[M[_], F[_], A, B, C](toA: => M[A],
                                          toB: => M[B],
                                          toC: (A, B) => C)
                                         (implicit nep: NonEmptyParallel[M]): Stream[M, C] = Stream.eval {
    val funcA = toA
    val funcB = toB
    (funcA, funcB).parMapN {
      case (a, b) =>
        toC(a, b)
    }
  }


  def run(args: List[String]): IO[ExitCode] = {

      randomMessageF /*[IO, IO.Par, String, String, String]*/ (
        IO(Random.nextInt(1000).toString),
        IO(Random.nextString(10)),
        (k: String, v: String) => s"$k:$v"
      ).compile.toList.flatMap(li => IO(println("unsafeRunSync().head"))).as(ExitCode(0))
  }




}
