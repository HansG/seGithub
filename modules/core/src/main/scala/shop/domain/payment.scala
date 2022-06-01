package shop.domain

import derevo.cats._
import derevo.circe.magnolia.{ decoder, encoder }
import derevo.derive
import shop.domain.auth.UserId
import shop.domain.checkout.Card
import squants.market.Money

object payment {

  @derive(encoder, decoder, show)
  case class Payment(
      id: UserId,
      total: Money,
      card: Card
  )

}
