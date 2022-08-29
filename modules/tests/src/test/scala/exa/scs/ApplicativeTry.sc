import cats.Invariant.catsApplicativeForArrow
import cats.syntax.either._

import scala.::
import scala.collection.immutable.:: // for catchOnly


def parseInt(str: String): Either[String, Int] =
  Either.catchOnly[NumberFormatException](str.toInt).
    leftMap(_ => s"Couldn't read $str")


for {
  a <- parseInt("a")
  b <- parseInt("b")
  c <- parseInt("c")
} yield (a + b + c)
// res0: Either[String, Int] = Left("Couldn't read a")


import cats.Semigroupal
import cats.instances.option._ // for Semigroupal
Semigroupal[Option].product(Some(123), Some("abc"))
Semigroupal[Option].product(None, Some("abc"))

Semigroupal.tuple3(Option(1), Option(2), Option(3))
Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])

Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)

import cats.syntax.apply._
(Option(123), Option("abc")).tupled
(Option(123), Option("abc"), Option(true)).tupled
(Option(123), Option("abc"), None).tupled

final case class Cat(name: String, born: Int, color: String)

( Option("Garfield"),
Option(1978),
Option("Orange & black")
).mapN(Cat.apply)

( Option("Garfield"), None, Option("Orange & black") ).mapN(Cat.apply)

val add: (Int, Int) => Int = (a, b) => a + b
//(Option(1), Option(2), Option(3)).mapN(add)
//(Option(4), Option(true)).mapN(add)

import cats.Monoid
import cats.instances.int._ // for Monoid
import cats.instances.invariant._ // for Semigroupal
import cats.instances.list._ // for Monoid
import cats.instances.string._ // for Monoid
import cats.syntax.apply._ // for imapN
final case class Cat( name: String, yearOfBirth: Int, favoriteFoods: List[String])
val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply _

val catToTuple: Cat => (String, Int, List[String]) = cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

implicit val catMonoid: Monoid[Cat] = (
  Monoid[String],
  Monoid[Int],
  Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

import cats.syntax.semigroup._ // for |+|
val garfield = Cat("Garfield", 1978, List("Lasagne"))
val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))
garfield |+| heathcliff

Semigroupal[List].product(List(1, 2), List(3, 4))

import cats.instances.either._ // for Semigroupal
type ErrorOr[A] = Either[Vector[String], A]
val error1: ErrorOr[Int] = Left(Vector("Error 1"))
val error2: ErrorOr[Int] = Left(Vector("Error 2"))
Semigroupal[ErrorOr].product(error1, error2)

import cats.syntax.apply._ // for tupled
import cats.instances.vector._ // for Semigroup on Vector
//( Left(Vector("Error 1")), Left(Vector("Error 2")) ).tupled

import cats.syntax.parallel._ // for parTupled
(error1, error2).parTupled

type ErrorOrList[A] = Either[List[String], A]
val errStr1: ErrorOrList[Int]  = Left(List("error 1"))
val errStr2: ErrorOrList[Int]  = Left(List("error 2"))
(errStr1, errStr2).parTupled
val errStr3: ErrorOrList[Int]  = Right(2)
(errStr1, errStr3).parTupled
val errStr4: ErrorOrList[Int]  = Right(4)
(errStr4, errStr3).parTupled

val l1: Either[String, Int]  = Left("error 1")
val l2: Either[String, Int]  = Left("error 2")
(l1, l2).parTupled

val success1: ErrorOr[Int] = Right(1)
val success2: ErrorOr[Int] = Right(2)
val addTwo = (x: Int, y: Int) => x + y
(error1, error2).parMapN(addTwo)
// res4: ErrorOr[Int] = Left(Vector("Error 1", "Error 2"))
(success1, success2).parMapN(addTwo)

val el1: List[String]  = List("error 1")
val el2: List[String]  = List("error 2")
(el1, el2).parTupled

(el1, el2).parMapN(_ +"\n"+ _)

import cats.arrow.FunctionK
object optionToList extends FunctionK[Option, List] {
  def apply[A](fa: Option[A]): List[A] =
    fa match {
      case None => List.empty[A]
      case Some(a) => List(a)
    }
}
val otl = optionToList(Some(1))
optionToList(None)






