package shop.programs


import cats.effect.kernel.MonadCancel
import cats.{FlatMap, Monad}
import cats.effect.{ExitCode, GenTemporal, IO, IOApp, Ref, Sync, Temporal}
import cats.effect.std.Console
import cats.syntax.all._

import collection.immutable.Queue
import scala.concurrent.duration.DurationInt


/**
 * Single producer - single consumer system using an unbounded concurrent queue.
 *
 * Second part of cats-effect tutorial at https://typelevel.org/cats-effect/tutorial/tutorial.html
 */
object InefficientProducerConsumerTry extends IOApp {

  //(implicit FT : Temporal[F]): Temporal
  // _ <-   Temporal[F].sleep(50.milliseconds)    (implicit T: Temporal[F])
  def test[F[_]: Sync: Console: Temporal ](counter: Int): F[Unit] =
    for {
      _ <- if(counter % 10000 == 0) Console[F].println(s"counter: ${counter}") else Sync[F].unit
      _ <- test(  counter + 1 )
    } yield ()

  def producer[F[_]: Sync: Console](queueR: Ref[F, Queue[Int]], counter: Int): F[Unit] = //(implicit FT : Temporal[F])
    for {
      _ <- if(counter % 10000 == 0) Console[F].println(s"Produced $counter items") else Sync[F].unit
      _ <- queueR.getAndUpdate(_.enqueue(counter + 1))
      _ <- producer(queueR, counter + 1)
    } yield ()

  def consumer[F[_] : Sync: Console](queueR: Ref[F, Queue[Int]]): F[Unit] =
    for {
      iO <- queueR.modify{ queue =>
        queue.dequeueOption.fold((queue, Option.empty[Int])){case (i,queue) => (queue, Option(i))}
      }
      _ <- if(iO.exists(_ % 10000 == 0)) Console[F].println(s"Consumed ${iO.get} items")
      else Sync[F].unit
      _ <- consumer(queueR)
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    for {
      queueR <- Ref.of[IO, Queue[Int]](Queue.empty[Int])
      res <- (consumer(queueR), producer(queueR, 0))
        .parMapN((_, _) => ExitCode.Success) // Run producer and consumer in parallel until done (likely by user cancelling with CTRL-C)
        .handleErrorWith { t =>
          Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)
        }
    } yield res

}
