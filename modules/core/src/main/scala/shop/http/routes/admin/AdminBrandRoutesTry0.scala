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

}
