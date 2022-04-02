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

import java.util.UUID
import shop.ext.refined._
//import eu.timepit.refined.api._
//import eu.timepit.refined.auto._

object XCheckValueDeEncode extends App {
  val decName = decoderOf[String, MatchesRegex[Rgx]].map(s => CardName(s.asInstanceOf[CardNamePred]))

//  object CardNumberX     extends RefinedTypeOps[CardNumberPred, Long]
//  object CardNameX       extends RefinedTypeOps[CardNamePred, String]
//  object CardExpirationX extends RefinedTypeOps[CardExpirationPred, String]
//  object CardCVVX        extends RefinedTypeOps[CardCVVPred, Int]

  //horizontale Kombi zu Produkttyp mit kumulierten/kombinierten/Produkt der Fehler:
  def toCardOrFails(name: String, number: Long, expiration: String, cvv: Int) =
    Parallel.parMap4(
      CardName.from(name),
      CardNumber.from(number),
      CardExpiration.from(expiration),
      CardCVV.from(cvv)
    )((na, nu, e, c) => Card(CardName(na), CardNumber(nu), CardExpiration(e), CardCVV(c)))

  private val card1: Either[String, Card] = toCardOrFails("John", 1234567890123456L, "4444", 333)
  println("Card: " + card1) //Card: Right(Card(John,1234567890123456,4444,333))

  private val card2: Either[String, Card] = toCardOrFails(" John", 12345678901234567L, "44445", 333)
  //Card: Left(Predicate failed: " John".matches("^[a-zA-Z]+(([',. -][a-zA-Z ])?[a-zA-Z]*)*$").
  // Predicate failed: Must have 16 digits.
  // Left predicate of (Must have 4 digits && isValidValidInt("44445")) failed: Predicate failed: Must have 4 digits.)
  println("Card: " + card2)

  val pay1 = Payment(UserId(UUID.randomUUID()), USD(5.10), card2.getOrElse(card1.getOrElse(???)))
 // Payment: Payment(daf6b0f0-7400-476d-83de-2178603c39c5,5.1 USD,Card(John,1234567890123456,4444,333))
  println("Payment: " + pay1)

  def toCardOrFirstFail(name: String, number: Long, expiration: String, cvv: Int) =
    for {
      name       <- CardName.from(name)
      number     <- CardNumber.from(number)
      expiration <- CardExpiration.from(expiration)
      cvv        <- CardCVV.from(cvv)
    } yield Card(CardName(name), CardNumber(number), CardExpiration(expiration), CardCVV(cvv))

  toCardOrFirstFail("John", 1234567890123456L, "4444", 333)



//Für Json En/Decode @derive(decoder, encoder,.. und import:
  import io.circe.parser.decode
  import io.circe.syntax._


  //Encode -> kompaktes bzw. formatiertes Json:
  val asJ = pay1.asJson.noSpaces
  //{"id":"2399b828-6dd5-448a-8ac9-11d20efd3f6d","total":5.1,"card":{"name":"John","number":1234567890123456,"expiration":"4444","cvv":333}}
  println(asJ)
  val asJ1 = pay1.asJson.spaces4SortKeys
  println(asJ1)

  //Decode zu Either[Error, A] -> fail fast mit erstem Fehler + "downstream Felder"
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
              |}""".stripMargin
    ,//Right(Payment(031acfc9-9967-4022-9635-66a946e9a433,57.1 USD,Card(John,1234567890123456,4444,333)))
    """{
              |  "id": "031acfc9-9967-4022-9635-66a946e9a433",
              |  "total": 57.1,
              |  "card": {
              |    "name": " John",
              |    "number": 123456789012345,
              |    "expiration": "444",
              |    "cvv": 333
              |  }
              |}""".stripMargin
    ,//Left(DecodingFailure(Predicate failed: " John".matches("^[a-zA-Z]+(([',. -][a-zA-Z ])?[a-zA-Z]*)*$")., List(DownField(name), DownField(card))))
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
    ,//Left(DecodingFailure(Left predicate of (Must have 4 digits && isValidValidInt("444")) failed: Predicate failed: Must have 4 digits., List(DownField(expiration), DownField(card))))
  """
      |{
      |    "card" : {
      |        "cvv" : 333,
      |        "expiration" : "4444",
      |        "name" : "John",
      |        "number" : 12345678901234567
      |    },
      |    "id" : "2a47b1f1-ffd8-44c0-bd81-020088c222ee",
      |    "total" : 5.1
      |}
      |""".stripMargin
    //  Left(DecodingFailure(Predicate failed: Must have 16 digits., List(DownField(number), DownField(card))))
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
