package exa.scs

import cats.effect.std.Random
import cats.effect.{ExitCode, IO, IOApp, Ref}

import scala.concurrent.duration.{DurationDouble, DurationInt}

object TestWithIOApp extends IOApp {
  def incr(ref: Ref[IO, Int]) = IO.sleep(0.5.second) >> ref.update(_ + 1)

  def poll(ref: Ref[IO, Int], sl: Int) = IO.sleep(sl.second) >> ref.get.flatMap(c => IO.println(s"Poller $sl: $c"))

  override def run(args: List[String]): IO[ExitCode] = run1.as(ExitCode.Success)

  val run1 = for {
    ref <- IO.ref(0)
    i   <- incr(ref).foreverM.start
    p1  <- poll(ref, 1).foreverM.start
    p2  <- poll(ref, 2).foreverM.start
    p3  <- poll(ref, 3).foreverM.start

    _ <- IO.sleep(10.seconds)
    _ <- p1.cancel >> p2.cancel >> p3.cancel >> i.cancel
  } yield ()

  val run2 =
    for {
      ctr <- IO.ref(0)

      wait = IO.sleep(1.second)
      poll = wait *> ctr.get

      _ <- poll.flatMap(IO.println(_)).foreverM.start
      _ <- poll.map(_ % 3 == 0).ifM(IO.println("fizz"), IO.unit).foreverM.start
      _ <- poll.map(_ % 5 == 0).ifM(IO.println("buzz"), IO.unit).foreverM.start

      _ <- (wait *> ctr.update(_ + 1)).foreverM.void
    } yield ()

}

object TestRunSleepPrint extends IOApp { //wegen "print >> IO.readLine" nicht möglich als test-Methode -> IOApp
  /* test("run sleepPrint") {
     for {
       rand <- Random.scalaUtilRandom[IO]

       // try uncommenting first one locally! Scastie doesn't like System.in
       name <- IO.print("Enter your name: ") >> IO.readLine
       //name <- IO.pure("Daniel")

       english <- sleepPrint("Hello", name, rand).foreverM.start
       french <- sleepPrint("Bonjour", name, rand).foreverM.start
       spanish <- sleepPrint("Hola", name, rand).foreverM.start

       _ <- IO.sleep(5.seconds)
       _ <- english.cancel >> french.cancel >> spanish.cancel
     } yield ()

   }*/

  def sleepPrint(word: String, name: String, rand: Random[IO[*]]) =
    for {
      delay <- rand.betweenInt(200, 700)
      _     <- IO.sleep(delay.millis)
      _     <- IO.println(s"$word, $name")
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      rand <- Random.scalaUtilRandom[IO]

      // try uncommenting first one locally! Scastie doesn't like System.in
      name <- IO.print("Enter your name: ") >> IO.readLine
      //name <- IO.pure("Daniel")

      english <- sleepPrint("Hello", name, rand).foreverM.start
      french <- sleepPrint("Bonjour", name, rand).foreverM.start
      spanish <- sleepPrint("Hola", name, rand).foreverM.start

      _ <- IO.sleep(5.seconds)
      _ <- english.cancel >> french.cancel >> spanish.cancel
    } yield ExitCode.Success

  }
}

