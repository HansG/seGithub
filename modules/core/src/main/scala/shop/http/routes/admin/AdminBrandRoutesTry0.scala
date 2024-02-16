package shop.http.routes.admin

import cats.MonadThrow
import cats.syntax.all._
import io.circe.JsonObject
import io.circe.syntax._
import org.http4s.{ HttpRoutes, _ }
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server._
import shop.domain.brand._
import shop.ext.http4s.refined._
import shop.http.auth.users.AdminUser
import shop.services.Brands

final case class AdminBrandRoutesTry0[F[_]: JsonDecoder: MonadThrow](
    brands: Brands[F]
) extends Http4sDsl[F] {

  private[admin] val prefixPath = "/brandsX"

  private val httpRoutes: HttpRoutes[F] =
    HttpRoutes.of {
      /*
    §try Aufruf: Uri.fromString(cfg.uri.value + "/v1").liftTo[IO].flatMap { uri =>
      client.run(   POST(brandParam/Name??, uri/"brandsX")    -> Server(hier):  req.asJsonDecode[A].attempt.flatMap
    einfacher (C:\se\prj\queue-server\src\main\scala\QueueServer.scala)
    case POST -> Root / "createQueue" / queueName =>  ...//Aufruf: Uri.fromString(s"http://$host:$port/createQueue/$queueName")  Request[IO](Method.POST, uri)
       */
      case req @ POST -> Root =>
        req.decodeR[BrandName] { bp =>
          brands.create(bp).flatMap { id => //.toDomain
            Created(Brand(id, bp))
            // Created(JsonObject.singleton("brand_id", id.asJson))
          }
        }
    }

  def routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )

  /*
  §try alternativ C:\se\prj\queue-server\src\main\scala\QueueServer.scala  main
   prefixPath bei case ..Root / "..."/ ..
   statt  req.asJsonDecode[A].attempt.flatMap  ->
   request.as[List[String]].flatMap...) *> Ok("Published")......
    Aufruf:Uri.fromString(s"http://$host:$port/publish/$queueName") ..client.expect[String]( Request[IO](Method.POST, uri).withEntity(list.map(_.toString)))

   C:\se\prj\http4s-laminar-stack\modules\backend\src\main\scala\example\backend\Routes.scala multipart upload:
     case request @ POST -> Root / "upload" =>
     ...implicitly[EntityDecoder[IO, multipart.Multipart[IO]]].decode(request, strict = true).value flatMap mp => Traverse[Vector].traverse(mp.parts)   { part =>
     Aufruf: https://www.freecodecamp.org/news/upload-files-with-html/#:~:text=To%20re-iterate%2C%20sending%20files%20with%20HTML%20requires%20three,request%E2%80%99s%20Content-Type%20to%20multipart%2Fform-data%20using%20the%20enctype%20attribute.
  <form method="post" enctype="multipart/form-data">
    <label for="file">File</label>
    <input id="file" name="file" type="file" />
    <button>Upload</button>
  </form>
 */

}
