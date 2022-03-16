package shop.ext

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.Size
import io.circe.{Decoder, Encoder}

object refined {

  implicit def validateSizeN[N <: Int, R](implicit w: ValueOf[N]): Validate.Plain[R, Size[N]] =
    Validate.fromPredicate[R, Size[N]](
      _.toString.size == w.value,
      _ => s"Must have ${w.value} digits",
      Size[N](w.value)
    )

  def decoderOf[T, P](implicit v: Validate[T, P], d: Decoder[T]): Decoder[T Refined P] =
    d.emap(refineV[P].apply[T](_))

  implicit def encoderOf[T, P](d: Encoder[T]): Encoder[T Refined P] =
    d.contramap(_.value)

}
