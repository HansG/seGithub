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

/*
def factorial(n: Int): WriterT[Id, String, Int] = slowly(
  if(n == 0)  1.writer("fact 0 = 1")
   else
      factorial(n - 1).map(f => n*f).mapBoth( (log, f) => (log+s"\nfact ${n} = $f", f))
)


val re = Await.result(Future.sequence(Vector(
  Future(factorial(5)),
  Future(factorial(5))
)), 5.seconds)
val tes = re.map( f => f.run)

tes.map(log_f =>  println(s"Fact: ${log_f._2}\nHist:\n"+log_f._1) )
*/

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._ // for pure
import cats.syntax.writer._ // for tell
import cats.instances.vector._ // for Monoid


type Logged[A] = WriterT[Id, Vector[String], A]
42.pure[Logged]

Vector("Message").tell

41.pure[Logged].flatMap( f =>  (f + 1).pure[Logged])


def factorial1(n: BigInt) : Logged[BigInt] =
  for  {
    ans <- if(n == 0) {
      BigInt(1).pure[Logged]
    } else {
      slowly(factorial1(  n - 1  ).map(_ *  n ))
    }
    _ <- Vector(s"fact $n $ans").tell
  } yield ans

val (log, res) = factorial1(5).run

val logs = Await.result(Future.sequence(Vector(
  Future(factorial1(50)),
  Future(factorial1(40))
)).map(_.map(_.written.mkString(", ")  )), 5.seconds)

println(logs.mkString("\nNext:\n"))
