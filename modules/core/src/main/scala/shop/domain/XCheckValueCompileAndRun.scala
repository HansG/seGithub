package shop.domain

import cats.data.{ EitherNel, Validated, ValidatedNel }
import Validated._
import eu.timepit.refined.cats.syntax.CatsRefinedTypeOps
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.numeric.{ Greater, Interval }
import eu.timepit.refined.{ W, refineV }
import eu.timepit.refined.generic._
import eu.timepit.refined.predicates.all.NonEmpty
import cats.effect._
import cats.implicits._
import eu.timepit.refined._
import eu.timepit.refined.api.{ Refined, _ }
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.Contains
import eu.timepit.refined.numeric.Greater
import eu.timepit.refined.types.string.NonEmptyString
import io.estatico.newtype.macros._
import shapeless._
import skunk.syntax.id
import skunk.syntax.id

//Überprüfung von Werten zur Compiletime - und Runtime
object XCheckValueCompileAndRun extends App {

  type Word = String Refined MatchesRegex[W.`"[a-zA-Z]*"`.T]
  //statt W...
  type Rgx   = "[a-zA-Z]*";
  type Word1 = String Refined MatchesRegex[Rgx]
  //Compiletime
  def lookup(username: Word): Option[Word] = None
  lookup("aeinstein")
  //einfach
  identity[Word]("aeinstein")
//für Laufzeit
  object Word extends RefinedTypeOps[Word, String] //MatchesRegex[Rgx]
  val p = Word.from("aeinstein"): Either[String, Word]
  //oder ohne explizites object:
  def castt[T](value: Any)(implicit T: Typeable[T]): Option[T] = T.cast(value)
  val p = castt[Word]("aeinstein")
  //siehe:
  def des[T](implicit T: Typeable[T]): String = T.describe
//sequentiell vs parallel siehe XCheckValueDeEncode

  type Username = String Refined Contains['g']
  //Compiletime
  def lookupu(username: Username): Option[Username] = None
  lookupu("aeingstein")
  // implicit def urlValidate: Validate [String, Username] =  ???
  //  implicit def toU(s : String) = autoRefineT[String, Username](s)

  type OneToTen = Int Refined Interval.Closed[W.`1`.T, W.`10`.T]
  //Compiletime
  def see(n: OneToTen): Option[OneToTen] = None
  see(10)
  object OneToTen extends RefinedTypeOps[OneToTen, Int] //eigens object nötig für eigene Refined...
  println(OneToTen.validate(50))

  //Runtime:
  val parseNE                             = refineV[NonEmpty]
  val res: Either[String, NonEmptyString] = parseNE("some runtime value")
  //kein eigens object (zB OneToTen)  nötig für Standard  Refined
  val res1: Either[String, NonEmptyString] = NonEmptyString.from("some runtime value")

  //für eigenen Typ - Runtime:
  type GTFive = Int Refined Greater[5]
  object GTFive extends RefinedTypeOps[GTFive, Int] // macht refineV[...]
  val number: Int                   = 33
  val res33: Either[String, GTFive] = GTFive.from(number)

  // Parallele validated Conversion
  case class MyType(a: NonEmptyString, b: GTFive)
  def validateE(a: String, b: Int): EitherNel[String, MyType] =
    (NonEmptyString.from(a).toEitherNel, GTFive.from(b).toEitherNel)
      .parMapN(MyType.apply)

  def validateV(a: String, b: Int): ValidatedNel[String, MyType] =
    (NonEmptyString.from(a).toValidatedNel, GTFive.from(b).toValidatedNel).mapN(MyType.apply)

  println(validateV("", 2))

  //Compiletime Check von M <: Int
  case class Residue[M <: Int](n: Int) extends AnyVal {
    def +(rhs: Residue[M])(implicit m: ValueOf[M]): Residue[M] =
      Residue((this.n + rhs.n) % valueOf[M])
  }
  val fiveModTen = Residue[10](5)
  val nineModTen = Residue[10](9)
  fiveModTen + nineModTen // OK == Residue[10](4)
  val fourModEleven = Residue[10](4)
  fiveModTen + fourModEleven // compiler error:

}
