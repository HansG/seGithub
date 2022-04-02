package shop.domain

import eu.timepit.refined.cats.syntax.CatsRefinedTypeOps
import eu.timepit.refined.string.{MatchesRegex, ValidInt}
import eu.timepit.refined.numeric.{Greater, Interval}
import eu.timepit.refined.boolean.{And, Not}
import eu.timepit.refined.collection.{Contains, Forall }
import eu.timepit.refined.{W, refineV}
import eu.timepit.refined.generic._
import eu.timepit.refined.predicates.all.{NonEmpty, Size}
import eu.timepit.refined.api.RefType.refinedRefType
import eu.timepit.refined.api.{Refined, _}
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import cats.effect._
import cats.data.{EitherNel, Validated, ValidatedNel}
import Validated._
import cats.implicits._
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

  //oder ohne explizites object, jedoch mit expliziten Typ T
  def ToRefined[T,RTP](implicit  rt: RefinedType.AuxT[RTP, T])  = new  RefinedTypeOps[RTP, T]
  val p3 = ToRefined[String, Word].from("aeinstein")

  //oder direkt als Methode mit expliziten Typ T und P
  def toRefined[T,P](t: T)(implicit valV:  Validate[T, P]): Either[String,  T Refined P] = refineV[P].apply[T](t)(valV)
  toRefined[String, MatchesRegex[Rgx]]("aeinstein")

  //oder ohne explizites object, jedoch mit expliziten Typ T und P
  implicit def ToRefined1[T,P](implicit  rt: RefinedType.AuxT[T Refined P, T])  = new  RefinedTypeOps[T Refined P, T]
  val p1 = ToRefined1[String,MatchesRegex[W.`"[a-zA-Z]*"`.T]].from("aeinstein")
  val p2 = ToRefined1[String, MatchesRegex[Rgx]].from("aeinstein")


  //mit cast zu Option[T]
  def castt[T](value: Any)(implicit T: Typeable[T]): Option[T] = T.cast(value)
  val pt = castt[Word]("aeinstein")
  //behebt:-> No default Typeable for parametrized type  Word
  implicit def refTypeable[T,P](implicit castT:  Typeable[T], valV:  Validate[T, P]): Typeable[T Refined P] =
    new Typeable[T Refined P] {
      def cast(t: Any): Option[T Refined P] = {
        if(t == null) None
        else  {
          castT.cast(t) match {
            case None => None
            case ot => refineV[P].apply[T](ot.get) match {
                        case Left(_) => None
                        case Right(rv) => Some(rv)
                      }
          }
        }
      }

      def describe: String = castT.describe + " validate: "+valV

    }

//weitere Beispiele:
  type Username = String Refined Contains['g']
  //Compiletime
  def lookupu(username: Username): Option[Username] = None
  lookupu("aeingstein")
 // lookupu("aeinstein") -> COmpilefehler
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

  //für  Runtime:
  type GTFive = Int Refined Greater[5]
  object GTFive extends RefinedTypeOps[GTFive, Int] // macht refineV[...]
  val number: Int                   = 33
  val res33: Either[String, GTFive] = GTFive.from(number)

  //vertikale Kombi
  type SizedInt = String Refined (Size[4] And ValidInt)
  type Username0 = String Refined (Contains['g'] And Size[4])

  // COmpile  funktioniert nicht todo RUntime
  type Username1 = Username Refined Size[5]
  type Username1a =  Refined[Refined[String, Not[Forall[Not[Equal['g']]]]], Size[5]]
  //Compiletime
  def lookupu1(username: Username1): Option[Username1] = None
//  lookupu1("aeing")


  //horizontale Kombi
  //sequentiell vs parallel siehe XCheckValueDeEncode
  // Parallele validated Conversion
  case class HProd(a: NonEmptyString, b: GTFive)
  def toHProd(a: String, b: Int): EitherNel[String, HProd] =
    (NonEmptyString.from(a).toEitherNel, GTFive.from(b).toEitherNel)
      .parMapN(HProd.apply)

  def toHProdV(a: String, b: Int): ValidatedNel[String, HProd] =
    (NonEmptyString.from(a).toValidatedNel, GTFive.from(b).toValidatedNel).mapN(HProd.apply)

  println(toHProdV("", 2))

  //Compiletime Check von M <: Int  vgl. "Refined" mit Scala3
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
