import cats.~>
import cats.data.OptionT

val optionToList: Option ~> List = new ~>[Option, List] { override def apply[A](o: Option[A]): List[A] = o.toList }
val optionT: OptionT[Option, Int] = OptionT.some(42)
optionT.mapK[List](optionToList)

val optionTl: OptionT[List, Int] = OptionT[List, Int](List(Some(2), Some(3), Some(4))) 
optionTl. flatMap(x => OptionT. when(x % 2 == 0)(x)) //res0: OptionT[List, Int] = OptionT(List(Some(2), None, Some(4)))
optionTl. flatMap(x => if(x % 2 == 0) OptionT[List, Int](List(Some(2*x),None,Some(3*x)))  else OptionT[List, Int](List(None)))

val optionTl1: OptionT[List, Int] = OptionT[List, Int](List(Some(42), None))
optionTl1. cata[String]("default", x => x. toString + "!") // List[String] = List(42!, default)

val optionTl2: OptionT[List, Int] = OptionT[List, Int](List(Some(23), None))
optionTl2.foldF(Nil)(v => List(v, v * 2)) //List[Int] = List(23, 46)