import cats.data.OptionT
import cats.implicits.catsSyntaxApplicativeId
import exa.CatsTry.TreeMonad._
import exa.CatsTry.TailRecTry._
import cats.syntax.functor._
import cats.syntax.flatMap._ // for flatMap

branch(leaf(100), leaf(200)).
  flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

val br1 = leaf(10)
      .flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
      .flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
      .flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
      .flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
      .flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

import cats.instances.list._ // for Monad
type ListOption[A] =  OptionT[List, A]
val result1: ListOption[Int] = OptionT(List(Option(10)))
// result1: ListOption[Int] = OptionT(List(Some(10)))
val result2: ListOption[Int] = 32.pure[ListOption]

val i3 = result1.flatMap( i => result2.map(i2 => i2 + i) )


type ErrorOr[A] = Either[String, A]
type ErrorOrOption[A] = OptionT[ErrorOr, A]

import cats.instances.either._ // for Monad
val a = 10.pure[ErrorOrOption]
// a: ErrorOrOption[Int] = OptionT(Right(Some(10)))
val b = 32.pure[ErrorOrOption]
val c = a.flatMap(x => b.map(y => x + y))


import scala.concurrent.Future
import cats.data.{EitherT, OptionT}
type FutureEither[A] = EitherT[Future, String, A]
type FutureEitherOption[A] = OptionT[FutureEither, A]

import cats.instances.future._ // for Monad
import cats.syntax.monad._ // for iterateWhileM
import cats.syntax.flatMap._ // For flatMap

import scala.concurrent.ExecutionContext.Implicits.global

val futureEitherOr: FutureEitherOption[Int] =
  for {
    a <- 10.pure[FutureEitherOption]
    b <- 32.pure[FutureEitherOption]
  } yield a + b

import scala.concurrent.Await
import scala.concurrent.duration._

await(futureEitherOr , 10 seconds)