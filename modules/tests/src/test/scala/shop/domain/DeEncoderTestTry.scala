package shop.domain

import eu.timepit.refined.api
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex
import io.circe.Decoder
import shop.domain.auth.UserId
import shop.domain.checkout.{Card, CardName}
import shop.domain.payment.Payment
import shop.http.auth.users.User
import shop.programs.CheckoutSuite.F
import squants.market.USD

import java.util.UUID

object DeEncoderTestTry {
import checkout._
  import shop.ext.refined._
  val decName: Decoder[CardNumber] =  decoderOf[String , MatchesRegex[Rgx]].map(s =>   CardNumber(s.asInstanceOf[CardNumberPred]))
  import eu.timepit.refined.auto._
  def id[T](v: T): T = v


  //Compilefehler:
 // Payment(UserId(UUID.randomUUID()), USD(5.10), Card( CardName(id[CardNamePred]("34John")), CardNumber(id[CardNumberPred](123456789012345699L)), CardExpiration(id[CardExpirationPred]("4444")), CardCVV(id[CardCVVPred](333)) ) )


}
