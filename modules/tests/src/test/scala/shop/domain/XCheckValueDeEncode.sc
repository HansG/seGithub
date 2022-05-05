import cats.Show
import cats.data.{NonEmptyList, Validated}
import shop.domain.XCheckValueDeEncode._
import shop.domain.PrinterTest
//import shop.domain.checkout.Card
 import  pprint.PPrinter

implicit def toShow[T]: Show[T] = Show.fromToString
def show[A: Show](x: A): String = implicitly[Show[A]] show x
//implicit def pprint =   PPrinter[C]((t, c) => Iterator("INSTANCE OF CLASS C"))
// implicit def pprinter[A] =  PPrinter((_, a) => show(a)  )


import cats.implicits._

val v1 = "error".invalid[Int]
val v2 = 123.valid[String]

val v1e =  v1.toValidatedNel
val v1ee = Validated.Invalid(NonEmptyList("error" , Nil))
val err = "error"
val v1ef = Validated.Invalid(NonEmptyList(err , Nil))

v2.toValidatedNel






//private val card1: Either[String, Card] = toCardOrFails("John", 1234567890123456L, "4444", 333)
//println("Card: " + card1) //Card: Right(Card(John,1234567890123456,4444,333))
