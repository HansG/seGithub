package shop.http.routes

import shop.services.Brands

import cats.Monad
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

final case class BrandRoutes[F[_]: Monad](
    brands: Brands[F]
) extends Http4sDsl[F] {

  private[routes] val prefixPath = "/download"

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    /* §try Aufruf: 
        Uri.fromString(cfg.uri.value + "/v1/brands").liftTo[IO].flatMap { uri =>
        client.run(GET(uri)).use { resp =>
     */
    case GET -> Root =>
      Ok(brands.findAll)
    /* §try    Alternativ: Rückgabe mit Stream: C:\se\prj\queue-server\src\main\scala\QueueServer.scala 
    case GET -> Root / "subscribe" / queueName => Ok( .....Stream.fromQueueUnterminated(queue):Stream[IO, String])  
    Aufruf: Uri.fromString(s"http://$host:$port/subscribe/queueName") ..client.stream(Request[IO](Method.GET, uri)).flatMap(_.body).through(text.utf8.decode).map(_.toLong)  
     */
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )

}
