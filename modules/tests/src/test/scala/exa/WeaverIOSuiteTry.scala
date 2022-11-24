package exa

import weaver._
import cats.effect._
import cats.effect.std.Random
import cats.effect.{ExitCode, IO, IOApp, Ref}

import scala.concurrent.duration.{DurationDouble, DurationInt}

//file:///C:/se/doc/Testing%20asynchronous%20pipelines%20with%20fs2%20and%20weaver-test%20_%20by%20Olivier%20Melois%20_%20disney-streaming%20_%20Medium.mhtml
object WeaverIOSuiteTry extends SimpleIOSuite {
  val uuid = IO(java.util.UUID.randomUUID())


  def incr(ref: Ref[IO, Int]) = IO.sleep(0.5.second) >> ref.update(_ + 1)

  def poll(ref: Ref[IO, Int], sl: Int) = IO.sleep(sl.second) >> ref.get.flatMap(c => IO.println(s"Poller $sl: $c"))

  override def run(args: List[String]): IO[ExitCode] = run2.as(ExitCode.Success)

  val run1 = for {
    ref <- IO.ref(0)
    i <- incr(ref).foreverM.start
    p1 <- poll(ref, 1).foreverM.start
    p2 <- poll(ref, 2).foreverM.start
    p3 <- poll(ref, 3).foreverM.start
  }






    test("UUID is random") {
    for {
      u1 <- uuid
      u2 <- uuid
    } yield expect(u1 != u2)
  }

  test("UUID always contains 36 characters") {
    uuid.map(u => expect(u.toString.size == 36))
  }


}