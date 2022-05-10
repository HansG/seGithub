package shop.domain

import cats._
import cats.implicits._
import cats.instances._
import NonEmptyParallel._
import cats.instances.EitherInstances
import cats.conversions.all._
import cats.data.implicits
import cats.data.Validated._
import shop.ext.refined._
import derevo.cats._
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.api._
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.Size
import eu.timepit.refined.predicates.all._
import eu.timepit.refined.string.{MatchesRegex, ValidInt}
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.{Decoder, Encoder, Json}
import io.estatico.newtype.macros.newtype
//import io.circe.refined._
//import eu.timepit.refined.cats._
//import shop.domain.auth.UserId
//import shop.domain.payment.Payment
//import cats.data.EitherNel
//import squants.market.USD

//import java.util.UUID

object checkout {
 

  //  type Rgx = "[a-zA-Z]*)*$"
  type Rgx = "^[a-zA-Z]+(([',. -][a-zA-Z ])?[a-zA-Z]*)*$"

  type CardNamePred       = String Refined MatchesRegex[Rgx]
  type CardNumberPred     = Long Refined Size[16]
  type CardExpirationPred = String Refined (Size[4] And ValidInt)
  type CardCVVPred        = Int Refined Size[3]

  @derive(decoder, encoder, show)
  @newtype
  case class CardName(value: CardNamePred)

  object CardNamePred extends RefinedTypeOps[CardNamePred, String]
  /*  object CardNamePred extends RefinedTypeOps[CardNamePred, String] {XX
    implicit val jsonDecoder: Decoder[CardName] =
      decoderOf[String, MatchesRegex[Rgx]].map( v => CardName(v))

    implicit val jsonEncoder: Encoder[CardName] =
      encoderOf[String, MatchesRegex[Rgx]].contramap(_.value)

  }*/





  @derive(decoder, encoder, show)
  @newtype
  case class CardNumber(value: CardNumberPred)

  object CardNumberPred extends RefinedTypeOps[CardNumberPred, Long] { //XX
    /*def apply(value: CardNumberPred)(implicit ev : CardNumberPred =:= Long Refined Object) = super.apply(value)
 implicit val jsonDecoder: Decoder[CardNumber] =
  decoderOf[Long, Size[16]].map(CardNumber(_))
   */
  }

  @derive(decoder, encoder, show)
  @newtype
  case class CardExpiration(value: CardExpirationPred)

  object CardExpirationPred extends RefinedTypeOps[CardExpirationPred, String] /*{XX
implicit val jsonDecoder: Decoder[CardExpiration] =
  decoderOf[String, Size[4] And ValidInt].map(CardExpiration(_))
}*/

  @derive(encoder, show)
  @newtype
  case class CardCVV(value: CardCVVPred)

  object CardCVV {
    //explizit falls Besonderheit nötig  wäre
    implicit val jsonDecoder: Decoder[CardCVV] =
      decoderOf[Int, Size[3]].map(CardCVV(_))
  }

  object CardCVVPred extends RefinedTypeOps[CardCVVPred, Int]

  @derive(decoder, encoder, show)
  case class Card(
      name: CardName,
      number: CardNumber,
      expiration: CardExpiration,
      cvv: CardCVV
  )

  //sequentielles Produkt-Parsen: Ergebnis Produkttyp oder erster Fehler (fail fast):
  object CardFF {
    def apply(name: String, number: Long, expiration: String, cvv: Int) =
      for {
        name       <- CardNamePred.from(name)
        number     <- CardNumberPred.from(number)
        expiration <- CardExpirationPred.from(expiration)
        cvv        <- CardCVVPred.from(cvv)
      } yield Card(CardName(name), CardNumber(number), CardExpiration(expiration), CardCVV(cvv))

  }

  //paralleles Produkt-Parsen: Ergebnis Produkttyp oder Produkt/Liste der Fehler:
  object CardFS {
    def apply(name: String, number: Long, expiration: String, cvv: Int) =
      Parallel.parMap4(
        CardNamePred.from(name).map(CardName(_)),
        CardNumberPred.from(number).map(CardNumber(_)),
        CardExpirationPred.from(expiration).map(CardExpiration(_)),
        CardCVVPred.from(cvv).map(CardCVV(_))
        //  )(Card)
      )(Card(_, _, _, _))
  }

  object CardFL {
    def apply(name: String, number: Long, expiration: String, cvv: Int) =
     (
        CardNamePred.from(name).toEitherNel,
        CardNumberPred.from(number).toEitherNel ,
        CardExpirationPred.from(expiration).toEitherNel,
        CardCVVPred.from(cvv).toEitherNel
        //  )(Card)
      ).parMapN((cna, cnu, ce, cv) => Card(CardName(cna),CardNumber(cnu), CardExpiration(ce), CardCVV(cv)))
  }
 /*
  type ValidatedS[A] = Validated[java.lang.String, A]
  implicit object NEPV extends cats.NonEmptyParallel[ValidatedS] {
    override type F[_] = String
    override def apply: Apply[F] = ???
    override def flatMap: FlatMap[ValidatedS] = ???
    override def sequential: ~>[F, ValidatedS] = ???
    override def parallel: ~>[ValidatedS, F] = ???
  }
*/


  object CardVL {
    def apply(name: String, number: Long, expiration: String, cvv: Int) =
     (
        CardNamePred.from(name).toValidatedNel,
        CardNumberPred.from(number).toValidatedNel ,
        CardExpirationPred.from(expiration).toValidatedNel,
        CardCVVPred.from(cvv).toValidatedNel
      ).parMapN((cna, cnu, ce, cv) => Card(CardName(cna),CardNumber(cnu), CardExpiration(ce), CardCVV(cv)))
  }
/*
  type ValidatedNS[A] = ValidatedNel[java.lang.String, A]
  implicit object NEPVN extends cats.NonEmptyParallel[ValidatedNS] {
    override type F[A] = List[A]
    override def apply: Apply[F] = ???
    override def flatMap: FlatMap[ValidatedNS] = ???
    override def sequential: ~>[F, ValidatedNS] = ???
    override def parallel: ~>[ValidatedNS, F] = ???
  }
*/
  object CardFU {
    def apply(name: String, number: String, expiration: String, cvv: String) =
      Card(
        CardName(Refined.unsafeApply(name)),
        CardNumber(Refined.unsafeApply(number.toLong)),
        CardExpiration(Refined.unsafeApply(expiration)),
        CardCVV(Refined.unsafeApply(cvv.toInt))
      )

  }

}
