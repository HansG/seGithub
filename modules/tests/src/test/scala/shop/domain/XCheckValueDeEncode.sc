import cats.Show
import shop.domain.XCheckValueDeEncode._
import shop.domain.StartEx
import shop.domain.checkout.Card
 import  pprint.PPrinter

implicit def toShow[T]: Show[T] = Show.fromToString
def show[A: Show](x: A): String = implicitly[Show[A]] show x
//implicit def pprint =   PPrinter[C]((t, c) => Iterator("INSTANCE OF CLASS C"))
// implicit def pprinter[A] =  PPrinter((_, a) => show(a)  )

private val card1: Either[String, Card] = toCardOrFails("John", 1234567890123456L, "4444", 333)
println("Card: " + card1) //Card: Right(Card(John,1234567890123456,4444,333))
