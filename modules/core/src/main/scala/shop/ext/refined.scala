package shop.ext

import cats.Show
import eu.timepit.refined._
import eu.timepit.refined.api.{ Refined, _ }
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.Size
import io.circe.{ Decoder, Encoder }

object refined {

  implicit def validateSizeN[N <: Int, R](implicit w: ValueOf[N]): Validate.Plain[R, Size[N]] =
    Validate.fromPredicate[R, Size[N]](
      _.toString.size == w.value,
      _ => s"Must have ${w.value} digits",
      Size[N](w.value)
    )

  implicit def decoderOf[T, P](implicit v: Validate[T, P], d: Decoder[T]): Decoder[T Refined P] =
    d.emap(refineV[P].apply[T](_))

  implicit def encoderOf[T, P](implicit d: Encoder[T]): Encoder[T Refined P] =
    d.contramap(_.value)

  implicit def showOf[T, P](implicit d: Show[T]): Show[T Refined P] =
    Show.show((rtp: T Refined P) => d.show(rtp.value))
  //Show[T].contraMap[T Refined P](_.value)
}
