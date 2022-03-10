package shop.domain

import eu.timepit.refined.{ api, refineV }
import eu.timepit.refined.api.Refined
import eu.timepit.refined.predicates.all.NonEmpty
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.all.NonEmptyString
import shop.domain.auth.UserId
import shop.domain.payment.Payment
import shop.http.auth.users.User
import squants.market.USD
import eu.timepit.refined._
import eu.timepit.refined.auto._
import shop.domain.checkout._

import java.util.UUID
import shop.ext.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.auto._

object XCheckValueDeEncode extends App {
  val decName = decoderOf[String, MatchesRegex[Rgx]].map(s => CardNumber(s.asInstanceOf[CardNumberPred]))

  // def id[T](t:T) = t

  // implicit def id[T](t:T) = t

  object CardNumberX extends RefinedTypeOps[CardNumberPred,Long ]
  object CardNameX extends RefinedTypeOps[CardNamePred, String]//MatchesRegex[Rgx]
  object CardExpirationX extends RefinedTypeOps[CardExpirationPred, String]//MatchesRegex[Rgx]
  object CardCVVX extends RefinedTypeOps[CardCVVPred, Int]//MatchesRegex[Rgx]

  private val cn          = 1234567890123456L
  private val card1: Either[String, Card] = Card.applyX(CardNameX.from("John"), CardNumberX.from(1234567890123456L), CardExpirationX.from("4444"), CardCVVX.from(333))
  val pay1 = Payment(UserId(UUID.randomUUID()), USD(5.10), card1.getOrElse(???))

  import io.circe.parser.decode
  import io.circe.syntax._

  val asJ = pay1.asJson.noSpaces
  println(asJ)

  val asJ2 =
    "{\"id\":\"237c9eed-d45d-4201-9077-c2b35b9346ef\",\"total\":5.1,\"card\":{\"name\":\"John\",\"number\":1234567890123456,\"expiration\":\"4444\",\"cvv\":3332}}"
  val jspay = """{
              |  "id": "031acfc9-9967-4022-9635-66a946e9a433",
              |  "total": 57.1,
              |  "card": {
              |    "name": "John",
              |    "number": 1234567890123456,
              |    "expiration": "4444",
              |    "cvv": 333
              |  }
              |}""".stripMargin

  val cardre = decode[Payment](jspay)
  println(cardre)

}
/*
 val str: String = "some runtime value"
  val checkNonEmpty = refineV[NonEmpty]
  val res: Either[String, NonEmptyString] = checkNonEmpty(str)
  val res1  = checkNonEmpty(List(3))
  val res2  = checkNonEmpty(Nil)
  println(res)
  println(res1)
  println(res2)
  type NEL[E] = List[E] Refined NonEmpty
  val l2 = List("2")
//  identity[NEL[String]](l2)
//  identity[NEL[Int]](Nil)
 */
