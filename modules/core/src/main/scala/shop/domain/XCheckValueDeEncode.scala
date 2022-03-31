package shop.domain

import cats.Parallel
import eu.timepit.refined.{ api, refineV }
import eu.timepit.refined.api.Refined
import eu.timepit.refined.predicates.all.NonEmpty
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.all.NonEmptyString
import shop.domain.auth.UserId
import shop.domain.checkout._
import shop.domain.payment.Payment
import shop.http.auth.users.User
import squants.market.USD
import shop.domain.checkout._

import java.util.UUID
import shop.ext.refined._
import eu.timepit.refined.api._
import shop.domain.XCheckValueDeEncode.{ CardCVVX, CardExpirationX, CardNumberX }
//import eu.timepit.refined.auto._

object XCheckValueDeEncode extends App {
  val decName = decoderOf[String, MatchesRegex[Rgx]].map(s => CardNumber(s.asInstanceOf[CardNumberPred]))

  // def id[T](t:T) = t

  // implicit def id[T](t:T) = t

  object CardNumberX     extends RefinedTypeOps[CardNumberPred, Long]
  object CardNameX       extends RefinedTypeOps[CardNamePred, String] //MatchesRegex[Rgx]
  object CardExpirationX extends RefinedTypeOps[CardExpirationPred, String] //MatchesRegex[Rgx]
  object CardCVVX        extends RefinedTypeOps[CardCVVPred, Int] //MatchesRegex[Rgx]

  def cardPar(name: String, number: Long, expiration: String, cvv: Int) =
    Parallel.parMap4(
      CardNameX.from(name),
      CardNumberX.from(number),
      CardExpirationX.from(expiration),
      CardCVVX.from(cvv)
    )((na, nu, e, c) => Card(CardName(na), CardNumber(nu), CardExpiration(e), CardCVV(c)))

  private val card1: Either[String, Card] = cardPar("John", 1234567890123456L, "4444", 333)
  println("Card: " + card1)
  private val card2: Either[String, Card] = cardPar(" John", 12345678901234567L, "44445", 333)
  println("Card: " + card2)
  val pay1 = Payment(UserId(UUID.randomUUID()), USD(5.10), card2.getOrElse(card1.getOrElse(???)))
  println("Payment: " + pay1)

  def cardSeq(name: String, number: Long, expiration: String, cvv: Int) =
    for {
      name       <- CardNameX.from(name)
      number     <- CardNumberX.from(number)
      expiration <- CardExpirationX.from(expiration)
      cvv        <- CardCVVX.from(cvv)
    } yield Card(CardName(name), CardNumber(number), CardExpiration(expiration), CardCVV(cvv))

  cardSeq("John", 1234567890123456L, "4444", 333)

  import io.circe.parser.decode
  import io.circe.syntax._

  val asJ = pay1.asJson.noSpaces
  println(asJ)

  val asJ2 =
    "{\"id\":\"237c9eed-d45d-4201-9077-c2b35b9346ef\",\"total\":5.1,\"card\":{\"name\":\"John\",\"number\":1234567890123456,\"expiration\":\"4444\",\"cvv\":3332}}"
  val payJs = List(
    """{
              |  "id": "031acfc9-9967-4022-9635-66a946e9a433",
              |  "total": 57.1,
              |  "card": {
              |    "name": "John",
              |    "number": 1234567890123456,
              |    "expiration": "4444",
              |    "cvv": 333
              |  }
              |}""".stripMargin,
    """{
              |  "id": "031acfc9-9967-4022-9635-66a946e9a433",
              |  "total": 57.1,
              |  "card": {
              |    "name": " John",
              |    "number": 123456789012345,
              |    "expiration": "444",
              |    "cvv": 333
              |  }
              |}""".stripMargin,
    """{
              |  "id": "031acfc9-9967-4022-9635-66a946e9a433",
              |  "total": 57.1,
              |  "card": {
              |    "name": "John",
              |    "number": 1234567890123456,
              |    "expiration": "444",
              |    "cvv": 333
              |  }
              |}""".stripMargin
  )

  payJs.foreach(js => println(decode[Payment](js)))

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
