import cats._
import cats.data._
import cats.implicits._


Reducible[NonEmptyList].reduce(NonEmptyList.of("a", "b", "c"))
// res0: String = "abc"
Reducible[NonEmptyList].reduceMap(NonEmptyList.of(1, 2, 4))(_.toString)
// res1: String = "124"
Reducible[NonEmptyVector].reduceK(NonEmptyVector.of(List(1,2,3), List(2,3,4)))
// res2: List[Int] = List(1, 2, 3, 2, 3, 4)
Reducible[NonEmptyVector].reduceLeft(NonEmptyVector.of(1,2,3,4))((s,i) => s + i)
// res3: Int = 10
Reducible[NonEmptyList].reduceRight(NonEmptyList.of(1,2,3,4))((i,s) => Later(s.value + i)).value
// res4: Int = 10
Reducible[NonEmptyList].reduceLeftTo(NonEmptyList.of(1,2,3,4))(_.toString)((s,i) => s + i)
// res5: String = "1234"
Reducible[NonEmptyList].reduceRightTo(NonEmptyList.of(1,2,3,4))(_.toString)((i,s) => Later(s.value + i)).value

Reducible[NonEmptyList].reduceRightTo(NonEmptyList.of(1,2,3,4))(_.toString)((i,s) => s.map(_ + i) )
res7.value