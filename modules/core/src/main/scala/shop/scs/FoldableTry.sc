import scala.:+

val l123 = List(1, 2, 3)
l123.foldLeft(0)(_ + _)
l123.foldRight(0)(_ + _)

1 :: Nil
l123.foldLeft(List[Int]())((l:List[Int], i) =>  new ::(i, l))
List(1).foldRight(0)(_ + _)
l123.foldRight(List[Int]())((i, l:List[Int]) =>  new ::(i, l))

List(1, 2, 3, 4, 5, 6, 7).foldRight(List[Int]())( (i,l:List[Int]) =>  if(i % 2 == 0 ) new ::(i, l) else l)

Some(1).foldLeft(4)(_ + _)
def sum(a: Int, b : Int) = a + b
None.foldLeft(4)(sum)


import cats.Eval
import cats.Foldable
import cats.data.Validated.{Invalid, Valid}

import scala.concurrent.{Await, Future}

def bigData = (1 to 100000).to(LazyList)
//bigData.foldRight(0L)(_ + _) java.lang.StackOverflowError ...

val eval: Eval[Long] =
  Foldable[LazyList].
    foldRight(bigData, Eval.now(0L)) { (num, eval) =>
      eval.map(_ + num)
    }
eval.value

val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
(Foldable[List] compose Foldable[Vector]).combineAll(ints)
import cats.syntax.foldable._ // for combineAll and foldMap
l123.combineAll
l123.foldMap(_.toString)

def sum[F[_]: Foldable](values: F[Int]): Int =
  values.foldLeft(0)(_ + _)

sum(l123)
sum(Vector(1,2,3))

val hostnames = List(
  "alpha.example.com",
  "beta.example.com",
  "gamma.demo.com"
)
import scala.concurrent.ExecutionContext.Implicits.global
def getUptime(hostname: String): Future[Int] =
  Future(hostname.length * 60)

val allUptimes: Future[List[Int]] =
  hostnames.foldLeft(Future(List.empty[Int])) {
    (accum, host) =>
      val uptime = getUptime(host)
      for {
        accuml <- accum
        uptime <- uptime
      } yield accuml :+ uptime
  }

val allUptimes1: Future[List[Int]] =
  Future.traverse(hostnames)(getUptime)

import scala.concurrent.duration._
Await.result(allUptimes, 1.second)

import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.apply._ // for mapN
def listTraverse[F[_]: Applicative, A, B]
(list: List[A])(func: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
    (accum, func(item)).mapN(_ :+ _)
  }

val totalUptime = listTraverse(hostnames)(getUptime)

def listSequence[F[_]: Applicative, B]
(list: List[F[B]]): F[List[B]] =
  listTraverse(list)(identity)

listSequence(List(Vector(1, 2), Vector(3, 4)))

(Vector(1, 2), Vector(3, 4)).mapN(_ + _)

import cats.implicits.catsSyntaxTuple2Parallel
(Vector(1, 2), Vector(3, 4)).parMapN(_ + _)

import cats.data.Validated
type ErrorsOr[A] = Validated[List[String], A]

def validate(inputs: List[Int]): ErrorsOr[List[Int]] =
  listTraverse(inputs) { n =>
    if(n % 2 == 0) {
      Validated.valid(n)
    } else {
      Validated.invalid(List(s"$n is not even"))
    }
  }
validate(List(2, 4, 6))
validate(List(2, 3, 4, 5, 6))

Applicative[ErrorsOr].map2(Valid(List(2,4)), Valid(3))(_ :+ _)
Applicative[ErrorsOr].map2(Valid(List(2,4)), Invalid(List("3 not even")))(_ :+ _)

Applicative[Vector].map2(Vector(1, 2), Vector(3, 4))( (_, _) )





