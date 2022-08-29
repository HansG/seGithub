import cats.{Applicative, Id}
import cats.implicits.toFunctorOps

import scala.concurrent.{Await, Future}
import scala.language.postfixOps

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

import cats.instances.future._ // for Applicative
//import cats.instances.list._ // for Traverse
import cats.syntax.traverse._ // for traverse
import scala.concurrent.ExecutionContext.Implicits.global

class UptimeService[F[_] ](client: UptimeClient[F])(implicit F : Applicative[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Int = hosts.getOrElse(hostname, 0)
}

class RealUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int] =
    Future.successful(hosts.getOrElse(hostname, 0))
}

val hosts = Map("host1" -> 10, "host2" -> 6)
val clientT = new TestUptimeClient(hosts)

abstract class FEqual[F[_]]{
  def isequal(a:F[Int], b: Int):Boolean
}
def testTotalUptime[F[_] ](client : UptimeClient[F])(implicit F : Applicative[F], E : FEqual[F]) = {
  val service = new UptimeService(client)
  val actual = service.getTotalUptime(hosts.keys.toList)
  val expected = hosts.values.sum
  E.isequal(actual , expected)
}

implicit object IdEqual extends FEqual[Id]{
  override def isequal(a: Id[Int], b: Int) = (a  == b)
}
testTotalUptime[Id](clientT)

val clientR = new RealUptimeClient(hosts)



import scala.concurrent.duration._
implicit object FutEqual extends FEqual[Future]{
  override def isequal(a: Future[Int], b: Int) = {
    val a0 = Await.result(a , 4 seconds)
    a0 == b
  }
}

val clientR = new RealUptimeClient(hosts)
testTotalUptime[Future](clientR)




testTotalUptime[Future](clientR)













