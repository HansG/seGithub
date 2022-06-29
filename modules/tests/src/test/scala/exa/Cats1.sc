import cats.Id
import cats.data.{Writer, WriterT}
import cats.syntax.all._
import cats.effect._
import fs2._

import scala.concurrent.duration._
import cats.syntax.all._

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Await, Future}

val job = IO.monotonic <* IO.sleep(10.seconds)


def slowly[A](body: => A) =
  try body finally Thread.sleep(100)

def factorial(n: Int): WriterT[Id, String, Int] = {
  val ans = slowly( if(n == 0)  1 else   n * factorial(n - 1) )
  Writer(s"fact $n = $ans", ans)
}


Await.result(Future.sequence(Vector(
  Future(factorial(5)),
  Future(factorial(5))
)), 5.seconds).run