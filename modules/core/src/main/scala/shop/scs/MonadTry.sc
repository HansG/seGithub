import scala.concurrent.Future
import cats.data.{EitherT, OptionT}
import cats.implicits.catsSyntaxApplicativeId
import shapeless.PolyDefns.->
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

//123.pure[EitherT[Option, String, *]]

type Response[A] = EitherT[Future, String, A]

val powerLevels = Map(
  "Jazz" -> 6,
  "Bumblebee" -> 8,
"Hot Rod" -> 10
)

def getPowerLevel(ally: String): Response[Int] = {
  powerLevels.get(ally) match {
    case Some(avg) => EitherT.right(Future(avg))
    case None => EitherT.left(Future(s"$ally unreachable"))
  }
}

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
