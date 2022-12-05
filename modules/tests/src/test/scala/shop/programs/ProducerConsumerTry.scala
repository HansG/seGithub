package shop.programs


import cats.effect.kernel.MonadCancel
import cats.{FlatMap, Monad}
import cats.effect.{ExitCode, GenTemporal, IO, IOApp, Ref, Sync, Temporal}
import cats.effect.std.{Console, Supervisor}
import cats.syntax.all._

import collection.immutable.Queue
import scala.concurrent.duration.DurationInt


/**
 * Single producer - single consumer system using an unbounded concurrent queue.
 *
 * Second part of cats-effect tutorial at https://typelevel.org/cats-effect/tutorial/tutorial.html
 */
object ProducerConsumerTry  {

  import cats.effect.{Deferred, Ref, Async}
  import cats.effect.std.Console
  import cats.syntax.all._
  import scala.collection.immutable.Queue

  case class State[F[_], A](queue: Queue[A], takers: Queue[Deferred[F,A]])

  def consumer[F[_]: Async: Console](id: Int, stateR: Ref[F, State[F, Int]]): F[Unit] = {
    val take: F[Int] =
      Deferred[F, Int].flatMap { taker =>
         stateR.modify(  state =>
            state.queue.dequeueOption.fold {
              (State(state.queue, state.takers.enqueue(taker) ), taker.get)
            }
            {
              case (i,queue) => (State(queue, state.takers), Async[F].pure(i))
            }
         ).flatten
      }

    for {
      i <- take
      _ <- if(i % 10000 == 0) Console[F].println(s"Consumer $id has reached $i items") else Async[F].unit
      _ <- consumer(id, stateR)
    } yield ()

  }

  }





object InefficientProducerConsumerTry extends IOApp {

  //(implicit FT : Temporal[F]): Temporal
  // _ <-   Temporal[F].sleep(50.milliseconds)    (implicit T: Temporal[F]): Temporal
  def test[F[_]: Sync: Console ](counter: Int): F[Unit] =
    for {
      _ <- if(counter % 10000 == 0) Console[F].println(s"counter: ${counter}") else Sync[F].unit
   //   _ <- Sync[F].blocking(Thread.sleep(500))
      _ <- test(  counter + 1 )
    } yield ()

  def producer[F[_]: Sync: Console](queueR: Ref[F, Queue[Int]], counter: Int): F[Unit] = //(implicit FT : Temporal[F])
    for {
      _ <- if(counter % 10000 == 0) Console[F].println(s"Produced $counter items") else Sync[F].unit
      _ <- queueR.getAndUpdate(_.enqueue(counter + 1))
 //     _ <- Sync[F].blocking(Thread.sleep(500))
      _ <- producer(queueR, counter + 1)
    } yield ()

  def consumer[F[_] : Sync: Console](queueR: Ref[F, Queue[Int]]): F[Unit] =
    for {
      iO <- queueR.modify{ queue =>
        queue.dequeueOption.fold((queue, Option.empty[Int])){case (i,queue) => (queue, Option(i))}
      }
      _ <- if(iO.exists(_ % 100 == 0)) Console[F].println(s"Consumed ${iO.get} items")
      else Sync[F].unit
  //    _ <- Sync[F].blocking(Thread.currentThread().wait(100))
      _ <- consumer(queueR)
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    for {
      queueR <- Ref.of[IO, Queue[Int]](Queue.empty[Int])
      _ <-  Supervisor[IO].use { supervisor =>
        supervisor.supervise( (consumer(queueR), producer(queueR, 0))
          .parMapN((_, _) => ExitCode.Success) // Run producer and consumer in parallel until done (likely by user cancelling with CTRL-C)
          .handleErrorWith { t =>
            Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)

      } )


        }
    } yield ExitCode.Success

}
