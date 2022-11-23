package exa

import weaver._
import cats.effect._

//file:///C:/se/doc/Testing%20asynchronous%20pipelines%20with%20fs2%20and%20weaver-test%20_%20by%20Olivier%20Melois%20_%20disney-streaming%20_%20Medium.mhtml
object WeaverIOSuiteTry extends SimpleIOSuite {
  val uuid = IO(java.util.UUID.randomUUID())

  test("UUID is random") {
    for {
      u1 <- uuid
      u2 <- uuid
    } yield expect(u1 != u2)
  }
  test("UUID always contains 36 characters") {
    uuid.map(u => expect(u.size == 36))
  }
}