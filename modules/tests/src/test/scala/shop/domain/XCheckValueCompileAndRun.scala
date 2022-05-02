package shop.domain

import cats.data.Validated._
import cats.data.{EitherNel, ValidatedNel}
import cats.implicits._
import eu.timepit.refined._
import eu.timepit.refined.api.RefType._
import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.boolean._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import eu.timepit.refined.char._
import eu.timepit.refined.collection._
import eu.timepit.refined.generic._
import eu.timepit.refined.cats.syntax._
import eu.timepit.refined.types.string.NonEmptyString
//import eu.timepit.refined.predicates.all.{And, Contains, Forall, Greater, GreaterEqual, Interval, LessEqual, LetterOrDigit, MatchesRegex, MaxSize, NonEmpty, Not, Positive, Size, Url, ValidFloat, ValidInt}
import io.estatico.newtype.macros.newtype
import shapeless._
import shop.domain.brand.Brand

//Überprüfung von Werten zur Compiletime - und Runtime
object XCheckValueCompileAndRun extends App {

  //https://tech.ovoenergy.com/safe-expressive-code-with-refinement-types/?msclkid=bde8b5c4a5e511ec9a9bdf04c8d62d95
  //Validierung zur Compiletime nur  mit (String-)Literal:
//  type NonEmpty = Not[Empty]
//  type NonEmptyString = String Refined NonEmpty

  val ne = refineMV[NonEmpty]("nm")
  val ne1 = refineMV[NonEmpty](" ")

  type CongT = String Refined Contains['g']
  type CongsT = String Refined (Contains['g'] And Size[4])
  val cg = refineMV[Contains['g']](" hgz")

  type SizedIntT  = String Refined (Size[4] And ValidInt)

  val y: String Refined Url = "http://example.com"

  val y1 = refineMV[Url]("http://example.com")

  refineMV[AnyOf[Digit :: Letter :: Whitespace :: HNil]]('F')
  // refineMV[MatchesRegex["[0-9]+"]]("123.")

  val d1: Char Refined Equal['3'] = '3'
  val d2: Char Refined Digit = d1
 // val d3: Char Refined Letter = d1


  val a: Int Refined Greater[5] = 10
  val b: Int Refined Greater[4] = a
 // val c: Int Refined Greater[6] = a

  type ZeroToOne = Not[Less[0.0]] And Not[Greater[1.0]]
//  val z1 = refineMV[ZeroToOne](1.8)



  type NameT  = String Refined (NonEmpty And MaxSize[20] And Forall[LetterOrDigit])
  type AlterT = Int Refined (GreaterEqual[7] And LessEqual[77])
  type Alter1T =  Int Refined Interval.ClosedOpen[7, 77]

  case class Person(name: NameT, alter: AlterT, url: String Refined Url)
  val per1 = Person("Peter", 58, "http://example.com")


  type WordT = String Refined MatchesRegex["[a-zA-Z]*"]

  val w1 = "aeinstein": WordT //Konstruktor , hier kein Either nötig

  val ps = List[WordT]("John", "adi")
 // val ps1 = List[WordT]("Joh n", "adi")
  case class Sentence(wds: WordT*)
  Sentence("John", "adi")
  // Sentence("J  ohn", "adi")   CF


  //für Laufzeit Either-Konstruktor
  val param = "aeinstein"
  val ne2 = NonEmptyString.from(param)

  val w0     = refineV[MatchesRegex["[a-zA-Z]*"]](param)

  //Laufzeit Either-Konstruktor ohne explizites object, jedoch mit expliziten Typ T und RTP
  def refine[T, RTP](implicit rt: RefinedType.AuxT[RTP, T]) = new RefinedTypeOps[RTP, T]
  val p3  = refine[String, WordT].from("aeinstein")
  // Companion-Objekt mit Konstruktoren
  object WordT extends RefinedTypeOps[WordT, String] //MatchesRegex[Rgx]
   val w     = WordT.from(param): Either[String, WordT]

  //falls z.B. für Testdaten kein Either erwünscht:
  def w2: WordT = Refined.unsafeApply(param) //Parameter Konvertierung möglich
  //damit Testdaten valide: -> Standard-Gen: vgl. modules/tests/src/main/scala/shop/generators.scala    z.B.
  //(Size[4] And ValidInt)  -> generators.sized(size: Int): Gen[Int]




  type MasszahlT = Double Refined Interval.Closed[0.0, 500.0]

  @newtype
  case class Celsius(num: MasszahlT)
  @newtype
  case class Fahrenheit(num: MasszahlT)

  case class Temperaturen(innen: Celsius, aussen: Fahrenheit)
  val temps = Temperaturen(Celsius(4.8), Fahrenheit(19.7))
 // val temps1 = Temperaturen(Fahrenheit(19.7), Celsius(4.8) )



  type Rgx = "[a-zA-Z]*"


  /* Beispiele für Standard Predicates:
  boolean: Not[P], Or[P1, P2], And[P1, P2], AllOf[Ps], AnyOf[Ps]
numeric: Greater[x], LessEqual[x], interval.OpenClosed[xMin, xMax]
collections: MinSize[x], Forall[P], Exists[P]
strings: EndsWith[s], MatchesRegex[s], Contains[s], Url, ValidFloat
   */
//Anwendungsbeispiele:

  //Predicate Produkt

  type OneToTen = Int Refined Interval.Closed[1, 10]
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
  type Congs5T  = CongsT Refined Size[5]
  type Congs5Ta = Refined[Refined[String, Not[Forall[Not[Equal['g']]]]], Size[5]]
  //Compiletime
  def lookupu1(username: Congs5T): Option[Congs5T] = None
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
