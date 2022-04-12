package shop.domain

import cats.effect.{ExitCode, IO, IOApp}
import cats.{Parallel, Show}
import org.scalacheck.Gen
import shop.domain.XCheckValueDeEncode.GenSum
import shop.domain.auth.UserId
import shop.domain.checkout._
import shop.domain.payment.Payment
import shop.http.routes.ExampleSuite
import squants.market.USD
import weaver.{SimpleIOSuite, TestOutcome}
import weaver.scalacheck.Checkers

import java.util.UUID
//import eu.timepit.refined.api._
//import eu.timepit.refined.auto._

object XCheckValueDeEncode extends App {

   val cn : CardNamePred = CardNamePred("Nawe")
 //  val cn1 = CardNameP(" nnn") Predicate failed

  //Laufzeit Parser/Konstruktor:

  //paralleles Produkt-Parsen: Ergebnis Produkttyp oder Produkt/Liste der Fehler:
  def toCardOrFails(name: String, number: Long, expiration: String, cvv: Int) =
    Parallel.parMap4(
      CardNamePred.from(name).map(CardName(_)),
      CardNumberPred.from(number).map(CardNumber(_)),
      CardExpirationPred.from(expiration).map(CardExpiration(_)),
      CardCVVPred.from(cvv).map(CardCVV(_))
  //  )(Card)
   )(Card(_,  _, _, _))

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

  //sequentielles Produkt-Parsen: Ergebnis Produkttyp oder erster Fehler (fail fast):
  def toCardOrFirstFail(name: String, number: Long, expiration: String, cvv: Int) =
    for {
      name       <- CardNamePred.from(name)
      number     <- CardNumberPred.from(number)
      expiration <- CardExpirationPred.from(expiration)
      cvv        <- CardCVVPred.from(cvv)
    } yield Card(CardName(name), CardNumber(number), CardExpiration(expiration), CardCVV(cvv))

  toCardOrFirstFail("John", 1234567890123456L, "4444", 333)


  // Summe-Gen
  object GenSum extends SimpleIOSuite  with Checkers {

    sealed trait A extends Product with java.io.Serializable
    case class B(b: String) extends A
    case class C(c: String) extends A


    val genS =  Gen.stringOf(Gen.oneOf(('a' to 'z') ++ ('A' to 'Z')))
    val genB =  genS.map (B(_))
    val genC =  genS.map (C(_))
    val genA = Gen.oneOf(genB, genC)

    implicit def toShow : Show[A] = Show.fromToString

    test("Show A's") {
      forall(genA) { a =>
        IO(println(a))
        expect.same("A","A")
      }
    }
  }




  //Laufzeit  Json En/Decode
  // via @derive(decoder, encoder,.. und import:
  import io.circe.parser.decode
  import io.circe.syntax._


  //Encode -> kompaktes  Json:
  val asJ = pay1.asJson.noSpaces
  //{"id":"2399b828-6dd5-448a-8ac9-11d20efd3f6d","total":5.1,"card":{"name":"John","number":1234567890123456,"expiration":"4444","cvv":333}}
  println(asJ)
  //Encode ->  formatiertes Json:
  val asJ1 = pay1.asJson.spaces4SortKeys
  println(asJ1)

  //Decode zu Either[Error, A] -> fail fast mit (erstem Fehler + "downstream Felder")
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



object StartEx extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = GenSum.run(List("a", "b")) { (to:TestOutcome) =>
    IO(println(to))
  }.as[ExitCode](ExitCode.Success)
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