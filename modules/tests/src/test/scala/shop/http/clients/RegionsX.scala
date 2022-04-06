package shop.http.clients

  import scala.concurrent.duration._
  import cats.effect._
  import cats.effect.std.{ Semaphore, Supervisor }


  object RegionsX  extends IOApp.Simple {
    def randomSleep(p: String) : IO[Unit] =
      IO(scala.util.Random.nextInt(100)).flatMap { ms =>
        IO.println(s"Sleep in $p on "+Thread.currentThread()) >> IO.sleep((ms + 700).millis)
      }.void

    def p1(sem: Semaphore[IO]): IO[Unit] =
      sem.permit.surround(IO.println("Running P1 on "+Thread.currentThread()) >>
        randomSleep("p1") )

    def p2(sem: Semaphore[IO]): IO[Unit] =
      sem.permit.surround(IO.println("Running P2 on "+Thread.currentThread()) >>
        randomSleep("p2"))

    def run: IO[Unit] =
      Supervisor[IO].use { s =>
        Semaphore[IO](1).flatMap { sem =>
          IO.println("Semaphore erzeugt  on "+Thread.currentThread()) >>
          s.supervise(p1(sem).foreverM).void *>
          s.supervise(p2(sem).foreverM).void *>
            IO.println("forever aufgerufen  on "+Thread.currentThread()) >>
          IO.sleep(5.seconds).void >>
            IO.println("fertig mit sleep   on "+Thread.currentThread())
        }
      }

    def run1: IO[Unit] =
        Semaphore[IO](1).flatMap { sem =>
          IO.println("Semaphore erzeugt  on "+Thread.currentThread()) >>
           p1(sem).foreverM.void *>
           p2(sem).foreverM.void *>
            IO.println("forever aufgerufen  on "+Thread.currentThread()) >>
          IO.sleep(5.seconds).void >>
            IO.println("fertig mit sleep   on "+Thread.currentThread())
        }

  }