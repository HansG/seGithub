package shop.domain

import cats.data.Validated._
import cats.data.{EitherNel, ValidatedNel}
import cats.implicits._
import eu.timepit.refined.{refineMV, refineV}
import eu.timepit.refined.api.RefType.refinedRefType
import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.generic._
import eu.timepit.refined.cats.syntax.CatsRefinedTypeOps
import eu.timepit.refined.predicates.all.{And, Contains, Forall, Greater, GreaterEqual, Interval, LessEqual, LetterOrDigit, MatchesRegex, MaxSize, NonEmpty, Not, Positive, Size, Url, ValidFloat, ValidInt}
import io.estatico.newtype.macros.newtype
import shapeless._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonEmptyString, NonNegInt}
import shop.domain.brand.Brand

//Überprüfung von Werten zur Compiletime - und Runtime
object XCheckValueCompileAndRun extends App {

  //https://tech.ovoenergy.com/safe-expressive-code-with-refinement-types/?msclkid=bde8b5c4a5e511ec9a9bdf04c8d62d95
  type NameT  = String Refined (NonEmpty And MaxSize[20] And Forall[LetterOrDigit])
  type AlterT = Int Refined GreaterEqual[0] And LessEqual[60]

  val y: String Refined Url = "http://example.com"

  case class Person(name: NameT, alter: AlterT, url: String Refined Url)
  val per1 = Person("Peter", 58, "http://example.com")

  type Rgx   = "[a-zA-Z]*"
  type WordT = String Refined MatchesRegex[Rgx]

  //Validierung zur Compiletime nur  mit String-Literal:
  val w1 = "aeinstein": WordT //Konstruktor , hier kein Either nötig

  //dasselbe ohne explizites Object Word
  //direkt mit Std-Methode identity: konvertiert String -> Word:
  val w3          = identity[WordT]("aeinstein")
  def id[A](a: A) = a
  //damit z.B.
  case class Sentence(wds: WordT*)
  Sentence("John", "adi")
  Sentence("J ohn", "adi")
  Sentence(id[WordT]("John"), id[WordT]("adi"))

//für Laufzeit Either-Konstruktor
// Companion-Objekt mit Konstruktoren
  object WordT extends RefinedTypeOps[WordT, String] //MatchesRegex[Rgx]
  val param = "aeinstein"
  val w     = WordT.from(param): Either[String, WordT]

  //falls z.B. für Testdaten kein Either erwünscht:
  def w2: WordT = Refined.unsafeApply(param) //Parameter Konvertierung möglich
  //damit Testdaten valide: -> Standard-Gen: vgl. modules/tests/src/main/scala/shop/generators.scala    z.B.
  //(Size[4] And ValidInt)  -> generators.sized(size: Int): Gen[Int]

  //für Laufzeit Either-Konstruktor  ohne explizites "object Word", direkt als Methode mit expliziten Typ T und P
  def toRefined[T, P](t: T)(implicit valV: Validate[T, P]): Either[String, T Refined P] = refineV[P].apply[T](t)(valV)
  val wtp                                                                               = toRefined[String, MatchesRegex[Rgx]]("aeinstein"): Either[String, WordT] //Word =:= String Refined MatchesRegex[Rgx] nötig!?

  //Laufzeit Either-Konstruktor ohne explizites object, jedoch mit expliziten Typ T und RTP
  def ToRefined[T, RTP](implicit rt: RefinedType.AuxT[RTP, T]) = new RefinedTypeOps[RTP, T]
  val p3                                                       = ToRefined[String, WordT].from("aeinstein")
  //dasselbe, jedoch mit expliziten Typ T und P ( RTP wird abgleitet -> T Refined P)
  implicit def ToRefined1[T, P](implicit rt: RefinedType.AuxT[T Refined P, T]) = new RefinedTypeOps[T Refined P, T]
  val p1                                                                       = ToRefined1[String, MatchesRegex["[a-zA-Z]*"]].from("aeinstein"): Either[String, WordT]
  val p2                                                                       = ToRefined1[String, MatchesRegex[Rgx]].from("aeinstein"): Either[String, WordT]

  def TR[T, P](t: T)(implicit rt: RefinedType.AuxT[T Refined P, T]) = ToRefined1[T, P].from(t)
  val p5                                                            = TR[String, MatchesRegex[Rgx]]("1aeinstein")

  @newtype
  case class cm(num: ValidFloat)
  @newtype
  case class inch(num: ValidFloat)
  case class Laenge(oben: cm, unten: inch)
  val laenge = Laenge(cm(4.8), inch(19.7))

  /* Beispiele für Standard Predicates:
  boolean: Not[P], Or[P1, P2], And[P1, P2], AllOf[Ps], AnyOf[Ps]
numeric: Greater[x], LessEqual[x], interval.OpenClosed[xMin, xMax]
collections: MinSize[x], Forall[P], Exists[P]
strings: EndsWith[s], MatchesRegex[s], Contains[s], Url, ValidFloat
   */
//Anwendungsbeispiele:
  type Username = String Refined Contains['g']
  //Compiletime
  identity[Username]("aeingstein")
  def lookupu(username: Username): Option[Username] = None
  lookupu("aeingstein")
  // lookupu("aeinstein") -> COmpilefehler
  // implicit def urlValidate: Validate [String, Username] =  ???
  //  implicit def toU(s : String) = autoRefineT[String, Username](s)

  //Predicate Produkt
  type SizedInt  = String Refined (Size[4] And ValidInt)
  type Username0 = String Refined (Contains['g'] And Size[4])

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

  // COmpile  funktioniert nicht todo RUntime
  type Username1  = Username Refined Size[5]
  type Username1a = Refined[Refined[String, Not[Forall[Not[Equal['g']]]]], Size[5]]
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

  //mit cast zu Option[T]
  def castt[T](value: Any)(implicit T: Typeable[T]): Option[T] = T.cast(value)
  val pt                                                       = castt[WordT]("aeinstein")
  //behebt:-> No default Typeable for parametrized type  Word
  implicit def refTypeable[T, P](implicit castT: Typeable[T], valV: Validate[T, P]): Typeable[T Refined P] =
    new Typeable[T Refined P] {
      def cast(t: Any): Option[T Refined P] = {
        if (t == null) None
        else {
          castT.cast(t) match {
            case None => None
            case ot =>
              refineV[P].apply[T](ot.get) match {
                case Left(_)   => None
                case Right(rv) => Some(rv)
              }
          }
        }
      }

      def describe: String = castT.describe + " validate: " + valV

    }

}
