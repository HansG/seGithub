package shop.programs



import cats.effect.std.Console
import cats.effect.{Async, ExitCode, IO, IOApp, Ref}
import cats.syntax.all._
import scala.concurrent.duration._


object MCQueueTest extends IOApp {


  private val console = IO.consoleForIO

  def producer(id: Int, counterR: Ref[IO, Int], cqueue:  MCQueue[IO , Int] ): IO[Unit] = {
    lazy val sizesF =  cqueue.sizes

    for {
      i <- counterR.getAndUpdate(_ + 1)
      _ <-   IO.sleep(50.millis)
      _ <- cqueue.offer(i)
      _ <- if (i % 1000 == 0) {
        sizesF.flatMap(s => console.println(s"Producer $id has reached $i items\n\t $s"))
      } else IO.unit
      _ <- producer(id, counterR, cqueue)
    } yield ()
  }



  def consumer(id: Int, cqueue: MCQueue[IO, Int]): IO[Unit] = {
    lazy val sizesF =   cqueue.sizes 

    for {
      i <- cqueue.take
      _ <-   IO.sleep(50.millis)
      _ <- if (i % 500 == 0)
        sizesF.flatMap(ss => console.println(s"Consumer $id has reached $i items\n\t$ss"))
      else IO.unit
      _ <- consumer(id, cqueue)
    } yield ()

  }

  def run(args: List[String]): IO[ExitCode] =
    for {
      cqueue <- MCQueue[IO, Int](10)
      counterR <- Ref.of[IO, Int](1)
      producers = List.range(1, 11).map(producer(_, counterR, cqueue)) // 10 producers
      consumers = List.range(1, 11).map(consumer(_, cqueue))           // 10 consumers
      res <- (producers ++ consumers).parSequence
        .as(ExitCode.Success) // Run producers and consumers in parallel until done (likely by user cancelling with CTRL-C)
        .handleErrorWith { t =>
          Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)
        }
    } yield res


  import java.util.concurrent.{Executors, TimeUnit}
  val scheduler = Executors.newScheduledThreadPool(1)
  def delay_(millis: Long) =
    Async[IO].async_[Unit] { cb =>
      scheduler.schedule(new Runnable {
        def run = cb(Right(()))
      }, millis, TimeUnit.MILLISECONDS)
      ()
    }
  // _ <- delay_(10)
  //     _ <- Sync[F].blocking(Thread.sleep(500))

/*
  _ <- Supervisor[IO].use { supervisor =>
    supervisor.supervise(
       IO[..]
    )
*/






}
