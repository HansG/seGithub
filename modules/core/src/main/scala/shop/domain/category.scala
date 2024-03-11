package shop.domain

import java.util.UUID
import shop.optics.{ IsUUID, uuid }
import derevo.cats._
import derevo.circe.magnolia.{ decoder, encoder }
import derevo.derive
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined._
import io.estatico.newtype.macros.newtype
import monocle.Iso

object category {
  @derive(decoder, encoder, eqv, show) //, uuid
  @newtype
  case class CategoryId(value: UUID)

  object CategoryId {
    implicit val identityProdId: IsUUID[CategoryId] = new IsUUID[CategoryId] {
      val _UUID = Iso[UUID, CategoryId](CategoryId(_))(_.value)
    }
  }
  @derive(decoder, encoder, eqv, show)
  @newtype
  case class CategoryName(value: String)

  @newtype
  case class CategoryParam(value: NonEmptyString) {
    def toDomain: CategoryName = CategoryName(value.toLowerCase.capitalize)
  }

  object CategoryParam {
    implicit val jsonDecoder: Decoder[CategoryParam] =
      Decoder.forProduct1("name")(CategoryParam.apply)
  }

  @derive(decoder, encoder, eqv, show)
  case class Category(uuid: CategoryId, name: CategoryName)
}
