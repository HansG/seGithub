import scala.concurrent.Future
import cats.data.{EitherT, OptionT}
import cats.implicits.catsSyntaxApplicativeId
import shapeless.PolyDefns.->

import scala.language.postfixOps
type FutureEither[A] = EitherT[Future, String, A]
type FutureEitherOption[A] = OptionT[FutureEither, A]

import cats.instances.future._ // for Monad
import cats.syntax.monad._ // for iterateWhileM
import cats.syntax.flatMap._ // For flatMap
import cats.instances.either._ // for Monad

import scala.concurrent.ExecutionContext.Implicits.global

val etf = EitherT[Future, String, Int] (Future( Right(24)))
val oef = OptionT(EitherT[Future, String, Option[Int]](Future{ Thread.sleep(1000); Right(Some(24))} ))
import scala.concurrent.Await
import scala.concurrent.duration._
val oefv = Await.result(oef.value.value, 4 seconds)

val futureEitherOr: FutureEitherOption[Int] =
  for {
    a <- 10.pure[FutureEitherOption]
    b <- 32.pure[FutureEitherOption]
    c <-  OptionT(EitherT (Future({Thread.sleep(1000); Right(Some(24)).asInstanceOf[Either[ String, Option[Int]]]})))
  } yield a + b + c


val feo =  futureEitherOr.value
val feo2 =  futureEitherOr.value.value
Await.result(feo2, 2.second)

import cats.data.Writer
type Logged[A] = Writer[List[String], A]
// Methods generally return untransformed stacks:
def parseNumber(str: String): Logged[Option[Int]] =
  util.Try(str.toInt).toOption match {
    case Some(num) => Writer(List(s"Read $str"), Some(num))
    case None => Writer(List(s"Failed on $str"), None)
  }
// Consumers use monad transformers locally to simplify composition:
def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
  import cats.data.OptionT
  val result = for {
    a <- OptionT(parseNumber(a))
    b <- OptionT(parseNumber(b))
    c <- OptionT(parseNumber(c))
  } yield a + b + c
  result.value
}
// This approach doesn't force OptionT on other users' code:
val result1 = addAll("1", "2", "3")
val result2 = addAll("1", "a", "3")

//S.140
type Response[A] = EitherT[Future, String, A]
val powerLevels = Map(
  "Jazz" -> 6,
  "Bumblebee" -> 8,
  "Hot Rod" -> 10
)
def getPowerLevel(autobot: String): Response[Int] = EitherT[Future, String, Int](Future(
  powerLevels.get(autobot).fold(Left(s"Not found $autobot").asInstanceOf[Either[String, Int]])(l => Right(l))  ) )

def getPowerLevel1(ally: String): Response[Int] = {
  powerLevels.get(ally) match {
    case Some(avg) => EitherT.right(Future(avg))
    case None => EitherT.left(Future(s"$ally unreachable"))
  }
}

getPowerLevel("Jazz")
getPowerLevel("Jazzz")
Await.result(getPowerLevel("Jazz").value, 1.second)
Await.result(getPowerLevel("Jazzz").value, 1.second)

//123.pure[EitherT[Option, String, *]]


def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
  for {
    p1 <- getPowerLevel(ally1)
    p2 <- getPowerLevel(ally2)
  } yield  (p1 + p2) > 15

Await.result(canSpecialMove("Jazz", "Hot Rod").value,1 seconds)
Await.result(canSpecialMove("Jazz", "Hot Rot").value,1 seconds)
Await.result(canSpecialMove("Jazzz", "Hot Rod").value,1 seconds)
Await.result(canSpecialMove("Jazzz", "Hot Rot").value,1 seconds)


def tacticalReport(ally1: String, ally2: String): String = {
  val resp = for {
    p <- canSpecialMove(ally1, ally2)
  } yield {
    val msg = if(p) s"OK für $ally1 und $ally2" else s"Sorry $ally1 und $ally2"
   // println(msg)
    msg
  }
  Await.result(resp.value,1 seconds).toString
}
tacticalReport("Jazz", "Hot Rod")
