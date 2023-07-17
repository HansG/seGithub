package shop.domain

import cats.implicits.toFunctorOps
import eu.timepit.refined.api
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
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




  //https://stackoverflow.com/questions/76437256/getting-java-lang-noclassdeffounderror-cats-flatmaparityfunctions-when-using-ci
  sealed trait SuperJob {

    def name: String

  }

  object SuperJob {

    implicit val encodeJob: Encoder[SuperJob] = Encoder.instance {
      case job @ FirstSuperJob(_)  => job.asJson
      case job @ SecondSuperJob(_) => job.asJson
    }

    implicit val decodeJob: Decoder[SuperJob] = List[Decoder[SuperJob]](
      Decoder[FirstSuperJob].widen,
      Decoder[SecondSuperJob].widen
    ).reduceLeft(_ or _)
  }

  case class FirstSuperJob(name: String) extends SuperJob

  object FirstSuperJob {

    implicit def decoder: Decoder[FirstSuperJob] = deriveDecoder
    implicit def encoder: Encoder[FirstSuperJob] = deriveEncoder

  }

  case class SecondSuperJob(name: String) extends SuperJob

  object SecondSuperJob {

    implicit def decoder: Decoder[SecondSuperJob] = deriveDecoder
    implicit def encoder: Encoder[SecondSuperJob] = deriveEncoder

  }

  object example extends App {
    val firstJob  = FirstSuperJob("firstjob")
    val secondJob = SecondSuperJob("secondjob")

    println(firstJob.name)
    println(secondJob.name)

    val firstJson = FirstSuperJob.encoder(firstJob)
    println(firstJson)

    val secondJson = SecondSuperJob.encoder(secondJob)
    println(secondJson)

    // Run time error below!!!!
    try {
      val firstDecodeResult = firstJson.as[FirstSuperJob]

      println(firstDecodeResult)

    } catch {
      case ex: Throwable => println(s"caught exception: ${ex}")
    }
  }
}
