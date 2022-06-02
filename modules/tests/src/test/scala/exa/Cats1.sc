import cats.syntax.all._
import cats.effect._
import fs2._
import scala.concurrent.duration._
import cats.syntax.all._

val job = IO.monotonic <* IO.sleep(10.seconds)
