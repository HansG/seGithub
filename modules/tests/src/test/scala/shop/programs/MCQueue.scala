package shop.programs

import cats.FlatMap
import cats.effect.std.Console
import cats.effect.{ Async, Deferred, ExitCode, IO, IOApp, Ref, Sync }

import scala.collection.immutable.Queue

import cats.syntax.all._
import cats.effect.syntax.all._



import cats.effect.{Async, Deferred, Ref}




private final case class State[F[_], A](
    queue: Queue[A],
    capacity: Int,
    takers: Queue[Deferred[F, A]],
    offerers: Queue[(A, Deferred[F, Unit])]
)

private object State {
  def empty[F[_], A](capacity: Int) =
    State[F, A](Queue.empty[A], capacity, Queue.empty[Deferred[F, A]], Queue.empty[(A, Deferred[F, Unit])])
}

class MCQueue[F[_]: Async: FlatMap, A](stateR: Ref[F, State[F, A]]) {

  val take: F[A] =
    for {
      taker <- Deferred[F, A]
      i <- Async[F].uncancelable { poll =>
        for {
          mod <- stateR.modify {
            case State(queue, c, takers, offerers) =>
              queue.dequeueOption.fold {
                offerers.dequeueOption.fold {
                  def cleanup = stateR.update(st => st.copy(takers = st.takers.filter(_ ne taker)))
                  State(queue, c, takers.enqueue(taker), offerers) -> poll(taker.get).onCancel(cleanup)
                } {
                  case ((i, offer), offerers) => State(queue, c, takers, offerers) -> offer.complete(()).as(i)
                }
              } {
                case (i, queue) =>
                  offerers.dequeueOption.fold {
                    State(queue, c, takers, offerers) -> Async[F].pure(i)
                  } {
                    case ((j, offer), offerers) =>
                      State(queue.enqueue(j), c, takers, offerers) -> offer.complete(()).as(i)
                  }
              }
          }
          i <- mod
        } yield i
      }
    } yield i

  def offer(a: A): F[Unit] =
    for {
      offer <- Deferred[F, Unit]
      _ <- Async[F].uncancelable { poll =>
        for {
          mod <- stateR.modify {
            case State(queue, c, takers, offerers) =>
              takers.dequeueOption.fold {
                if (queue.size < c) State(queue.enqueue(a), c, takers, offerers) -> Sync[F].unit
                else {
                  val cleanup = stateR.update(st => st.copy(offerers = st.offerers.filter(_ ne offer)))
                  State(queue, c, takers, offerers.enqueue((a, offer))) -> poll(offer.get).onCancel(cleanup)
                }
              } {
                case (taker, ntakers) => State(queue, c, ntakers, offerers) -> taker.complete(a).void
              }
          }
          _ <- mod
        } yield ()
      }
    } yield ()

  val sizes  = stateR.get.map(state => s"Queues Größe: ${state.queue.size}, Taker Größe: ${state.takers.size}, Offers Größe: ${state.offerers.size} ")


}


object MCQueue {
  def apply[F[_]: Async, A](capacity: Int): F[MCQueue[F, A]] =
    Ref.of[F, State[F, A]](State.empty[F, A](capacity)).map(st => new MCQueue(st))


}



