import scala.concurrent.Future
import cats.data.{EitherT, OptionT}
import cats.implicits.catsSyntaxApplicativeId
type FutureEither[A] = EitherT[Future, String, A]
type FutureEitherOption[A] = OptionT[FutureEither, A]

import cats.instances.future._ // for Monad
import cats.syntax.monad._ // for iterateWhileM
import cats.syntax.flatMap._ // For flatMap
import cats.instances.either._ // for Monad

import scala.concurrent.ExecutionContext.Implicits.global

val etf = EitherT[Future, String, Int] (Future( Right(24)))
val oef = OptionT(EitherT[Future, String, Option[Int]](Future{ Thread.sleep(180000); Right(Some(24))} ))
import scala.concurrent.Await
import scala.concurrent.duration._
val oefv = oef.value.value.result(4 seconds)

val futureEitherOr: FutureEitherOption[Int] =
  for {
    a <- 10.pure[FutureEitherOption]
    b <- 32.pure[FutureEitherOption]
    c <-  OptionT(EitherT (Future({Thread.sleep(180000); Right(Some(24)).asInstanceOf[Either[ String, Option[Int]]]})))
  } yield a + b + c


val feo =  futureEitherOr.value
val feo2 =  futureEitherOr.value.value

//123.pure[EitherT[Option, String, *]]