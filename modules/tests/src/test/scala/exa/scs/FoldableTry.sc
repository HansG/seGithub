import cats.{Monoid, NonEmptyParallel}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.toTraverseOps

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

def getUptimei(hostname: String): IO[Int] =
  IO(hostname.length * 60)

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

val allUptimes1i: IO[List[Int]] =
   hostnames.traverse(getUptimei)

import scala.concurrent.duration._
Await.result(allUptimes, 1.second)
import cats.effect.unsafe.implicits.global
val allup = allUptimes1i.unsafeRunTimed(1.second)

import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.apply._ // for mapN

def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
    (accum, func(item)).mapN(_ :+ _)
  }

val totalUptime = listTraverse(hostnames)(getUptimei)

def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] = listTraverse(list)(identity)

listSequence(List(Vector(1, 2), Vector(3, 4)))

(Vector(1, 2), Vector(3, 4)).mapN(_ + _)

import cats.implicits.catsSyntaxTuple2Parallel
(Vector(1, 2), Vector(3, 4)).parMapN(_ + _)
(Vector(1), Vector(3, 4)).parMapN(_ + _)

def listTraversePar[F[_]: Applicative : NonEmptyParallel, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
    (accum, func(item)).parMapN(_ :+ _)
  }
def listSequencePar[F[_]: Applicative : NonEmptyParallel, B](list: List[F[B]]): F[List[B]] = listTraversePar(list)(identity)
listSequencePar(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))


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

def amap2(a : ErrorsOr[List[Int]], b : ErrorsOr[Int]) = Applicative[ErrorsOr].map2(a,b)(_ :+ _)
amap2(Invalid(List("1 not even", "5 not even")), Invalid(List("3 not even")))

def amap2p(a : ErrorsOr[Int], b : ErrorsOr[Int]) = Applicative[ErrorsOr].map2(a,b)(_ + _)
amap2p(Valid(4), Valid(3))
amap2p(Invalid(List("1 not even", "5 not even")), Invalid(List("3 not even")))

Applicative[Vector].map2(Vector(1, 2), Vector(3, 4))( (_, _) )
Applicative[ErrorsOr].map2[List[Int], Int, List[Int]](Invalid(List("1 not even", "5 not even")), Invalid(List("3 not even")))(_ :+ _)


//import cats.instances.int._  for Monoid
Foldable[Vector].foldMap(Vector(1, 2, 3))(identity)
Foldable[Vector].foldMap("Hello world!".toVector)(_.toString.toUpperCase)

import cats.instances.list._ // for Traverse
import cats.syntax.traverse._ // for sequence
def parallelFoldMap[A, B : Monoid](values: Vector[A])(func: A => B): Future[B] =
  values.grouped(values.size / 8).toList.map( v => Future(Foldable[Vector].foldMap(v)(func))).
    sequence.map(lb => lb.foldMap(identity ))

val result: Future[Int] =
  parallelFoldMap((1 to 100000).toVector)(identity)
val gsum = Await.result(result, 1.second)

Foldable[Vector].foldMap((1 to 100).toVector)(identity)
(1 to 100).toList.foldMap(identity)

def parallelFoldMap1[A, B: Monoid](values: Vector[A])
(func: A => B): Future[B] = {
  val numCores = Runtime.getRuntime.availableProcessors
  val groupSize = (1.0 * values.size / numCores).ceil.toInt
  values
    .grouped(groupSize)
    .toVector
    .traverse(group => Future(group.toVector.foldMap(func)))
    .map(_.combineAll)
}
val future1: Future[Int] =
  parallelFoldMap((1 to 100000).toVector)(identity)
Await.result(future1, 1.second)
