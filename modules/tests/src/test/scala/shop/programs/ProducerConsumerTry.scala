package shop.programs

import cats.data.StateT
import cats.effect.kernel.MonadCancel
import cats.{FlatMap, Monad, effect}
import cats.effect.{ExitCode, GenTemporal, IO, IOApp, Ref, Sync, Temporal}
import cats.effect.std.{Console, Supervisor}
import cats.syntax.all._

import collection.immutable.Queue
import scala.concurrent.duration.DurationInt
import cats.effect.{Async, Deferred, Ref}
import cats.effect.std.Console
import cats.syntax.all._

import scala.collection.immutable.Queue

/**
  * Single producer - single consumer system using an unbounded concurrent queue.
  *
  * Second part of cats-effect tutorial at https://typelevel.org/cats-effect/tutorial/tutorial.html
  */
object ProducerConsumerTry extends IOApp {


  case class State[F[_], A](
      queue: Queue[A],
      capacity: Int,
      takers: Queue[Deferred[F, A]],
      offerers: Queue[(A, Deferred[F, Unit])]
  )

  object State {
    def empty[F[_], A](capacity: Int) =
      State(Queue.empty[A], capacity, Queue.empty[Deferred[F, A]], Queue.empty[(A, Deferred[F, Unit])])
  }


  def sizes[F[_]: FlatMap](stateR: Ref[F, State[F, Int]]) =  
     stateR.get.map(state => s"Queues Größe: ${state.queue.size}, Taker Größe: ${state.takers.size}, Offers Größe: ${state.offerers.size} ")

  def consumer[F[_]: Async: Console](id: Int, stateR: Ref[F, State[F, Int]]): F[Unit] = {
    val take: F[Int] =
      for {
        taker <- Deferred[F, Int]
        mod <- stateR.modify {
          case State(queue, c, takers, offerers) =>
            queue.dequeueOption.fold {
              offerers.dequeueOption.fold {
                State(queue, c, takers.enqueue(taker), offerers) -> taker.get
              } {
                case ((i, offer), offerers) => State(queue, c, takers, offerers) -> offer.complete(()).as(i)
              }
            } {
              case (i, queue) =>
                offerers.dequeueOption.fold {
                  State(queue, c, takers, offerers) -> Async[F].pure(i)
                } {
                  case ((j, offer), offerers) => State(queue.enqueue(j), c, takers, offerers) -> offer.complete(()).as(i)
                }
            }
        }
        i <- mod
      } yield i

    lazy val sizesF = sizes(stateR)

    for {
      i <- take
      _ <- if (i % 500 == 0)
        sizesF.flatMap(ss => Console[F].println(s"Consumer $id has reached $i items\n\t$ss"))
      else Async[F].unit
      _ <- consumer(id, stateR)
    } yield ()

  }

  def producer[F[_]: Sync: Async:  Console](id: Int, counterR: Ref[F, Int], stateR: Ref[F, State[F, Int]]): F[Unit] = {

    def offer(i: Int): F[Unit] =
      for {
        offer <- Deferred[F, Unit]
        _ <- Async[F].uncancelable { poll =>
          for {
            mod <- stateR.modify {
              case State(queue, c, takers, offerers) =>
                takers.dequeueOption.fold {
                  if (queue.size < c) State(queue.enqueue(i), c, takers, offerers) -> Sync[F].unit
                  else State(queue, c, takers, offerers.enqueue((i, offer)))       -> poll(offer.get).onCancel(cleanup)
                } {
                  case (taker, ntakers) => State(queue, c, ntakers, offerers) -> taker.complete(i).void
                }
            }
            _ <- mod
          } yield ()
        }
      } yield ()


    lazy val sizesF = sizes(stateR)

    for {
      i <- counterR.getAndUpdate(_ + 1)
      _ <- offer(i)
      // _ <- delay_(1)
      _ <- if (i % 1000 == 0)
        sizesF.flatMap(s => Console[F].println(s"Producer $id has reached $i items\n\tQueues Größe: $s"))
      else Sync[F].unit
      _ <- producer(id, counterR, stateR)
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      stateR <- Ref.of[IO, State[IO, Int]](State.empty[IO, Int](capacity = 100))

      counterR <- Ref.of[IO, Int](1)
      producers = List.range(1, 11).map(producer(_, counterR, stateR)) // 10 producers
      consumers = List.range(1, 11).map(consumer(_, stateR))           // 10 consumers
      res <- (producers ++ consumers).parSequence
        .as(ExitCode.Success) // Run producers and consumers in parallel until done (likely by user cancelling with CTRL-C)
        .handleErrorWith { t =>
          Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)
        }
    } yield res

  import java.util.concurrent.{ Executors, TimeUnit }
  val scheduler = Executors.newScheduledThreadPool(1)
  def delay_[F[_]: Async](millis: Long) =
    Async[F].async_[Unit] { cb =>
      scheduler.schedule(new Runnable {
        def run = cb(Right(()))
      }, millis, TimeUnit.MILLISECONDS)
      ()
    }

}














object InefficientProducerConsumerTry extends IOApp {

  //(implicit FT : Temporal[F]): Temporal
  // _ <-   Temporal[F].sleep(50.milliseconds)    (implicit T: Temporal[F]): Temporal
  def test[F[_]: Sync: Console](counter: Int): F[Unit] =
    for {
      _ <- if (counter % 10000 == 0) Console[F].println(s"counter: ${counter}") else Sync[F].unit
      //   _ <- Sync[F].blocking(Thread.sleep(500))
      _ <- test(counter + 1)
    } yield ()

  def producer[F[_]: Sync: Console](queueR: Ref[F, Queue[Int]], counter: Int): F[Unit] = //(implicit FT : Temporal[F])
    for {
      _ <- if (counter % 10000 == 0) Console[F].println(s"Produced $counter items") else Sync[F].unit
      _ <- queueR.getAndUpdate(_.enqueue(counter + 1))
      //     _ <- Sync[F].blocking(Thread.sleep(500))
      _ <- producer(queueR, counter + 1)
    } yield ()

  def consumer[F[_]: Sync: Console](queueR: Ref[F, Queue[Int]]): F[Unit] =
    for {
      iO <- queueR.modify { queue =>
        queue.dequeueOption.fold((queue, Option.empty[Int])) { case (i, queue) => (queue, Option(i)) }
      }
      _ <- if (iO.exists(_ % 100 == 0)) Console[F].println(s"Consumed ${iO.get} items")
      else Sync[F].unit
      //    _ <- Sync[F].blocking(Thread.currentThread().wait(100))
      _ <- consumer(queueR)
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    for {
      queueR <- Ref.of[IO, Queue[Int]](Queue.empty[Int])
      _ <- Supervisor[IO].use { supervisor =>
        supervisor.supervise(
          (consumer(queueR), producer(queueR, 0))
            .parMapN((_, _) => ExitCode.Success) // Run producer and consumer in parallel until done (likely by user cancelling with CTRL-C)
            .handleErrorWith { t =>
              Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)

            }
        )

      }
    } yield ExitCode.Success

}
