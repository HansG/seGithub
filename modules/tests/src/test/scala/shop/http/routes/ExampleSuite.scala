package shop.http.routes


import weaver._
import cats.effect._


object ExampleSuite extends SimpleIOSuite {
  val uuid = IO(java.util.UUID.randomUUID())

  test("UUID is random") {
    for {
      u1 <- uuid
      u2 <- uuid
      _ <-  IO(println(u1.toString +" "+u2.toString))
    } yield  expect(u1 != u2)
  }



  test("UUID always contains 36 characters") {
    uuid.map(u => expect(u.toString.size == 36))
  }


}

object StartEx extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = ExampleSuite.run(List("a", "b")) { (to:TestOutcome) =>
    IO(println(to))
  }.as[ExitCode](ExitCode.Success)
}