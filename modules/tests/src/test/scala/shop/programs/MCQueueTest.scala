package shop.programs



import cats.effect.std.Console
import cats.effect.{ Async,  ExitCode, IO, IOApp, Ref, Sync }

object MCQueueTest extends IOApp {



  def producer(id: Int, counterR: Ref[IO, Int], cqueue: MCQueue[IO, Int]): IO[Unit] = {
    lazy val sizesF =  cqueue.sizes

    for {
      i <- counterR.getAndUpdate(_ + 1)
      _ <- cqueue.offer(i)
      // _ <- delay_(1)
      _ <- if (i % 1000 == 0) {
        sizesF.flatMap(s => Console[IO].println(s"Producer $id has reached $i items\n\tQueues Größe: $s"))
      } else Sync[IO].unit
      _ <- producer(id, counterR, cqueue)
    } yield ()
  }

  def consumer(id: Int, cqueue: MCQueue[IO, Int]): IO[Unit] = {
    lazy val sizesF =   cqueue.sizes 

    for {
      i <- cqueue.take
      _ <- if (i % 500 == 0)
        sizesF.flatMap(ss => Console[IO].println(s"Consumer $id has reached $i items\n\t$ss"))
      else Async[IO].unit
      _ <- consumer(id, cqueue)
    } yield ()

  }

  def run(args: List[String]): IO[ExitCode] =
    for {
      cqueue <- MCQueue[IO, Int].apply(10)
      counterR <- Ref.of[IO, Int](1)
      producers = List.range(1, 11).map(producer(_, counterR, cqueue)) // 10 producers
      consumers = List.range(1, 11).map(consumer(_, cqueue))           // 10 consumers
      res <- (producers ++ consumers).parSequence
        .as(ExitCode.Success) // Run producers and consumers in parallel until done (likely by user cancelling with CTRL-C)
        .handleErrorWith { t =>
          Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)
        }
    } yield res

}
