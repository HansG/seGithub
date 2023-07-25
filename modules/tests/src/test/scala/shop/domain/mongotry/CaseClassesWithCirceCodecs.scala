/*
 * Copyright 2020 Kirill5k
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shop.domain.mongotry

import cats.Show
import cats.conversions.all.autoNarrowContravariant
import cats.data.Validated
import cats.effect.{IO, Resource}
import cats.implicits.{catsSyntaxTuple2Semigroupal, catsSyntaxTuple4Semigroupal, toContravariantOps}
import derevo.cats.{eqv, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.api.{Refined, RefinedTypeOps, Validate}
import eu.timepit.refined.collection.{Forall, Size}
import eu.timepit.refined.refineV
import eu.timepit.refined.auto._
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.predicates.all.MaxSize
import io.circe.{CursorOp, Decoder, DecodingFailure, Encoder, HCursor, Json, JsonObject}
import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import io.circe.parser.decode
import io.circe.syntax._
import mongo4cats.bson.ObjectId
import mongo4cats.client.MongoClient
import mongo4cats.circe.{instantEncoder => _, instantDecoder => _,   _}
import mongo4cats.database.GenericMongoDatabase
import mongo4cats.operations.Filter
import munit.CatsEffectSuite
import org.bson.codecs.configuration.{CodecRegistries, CodecRegistry}
import shop.domain.cart.Cart
import skunk.syntax.id
import weaver.Log.info

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.language.postfixOps
import scala.util.{Success, Try}
import fs2.Stream


/*
MongoSocketOpenException: Exception opening socket  connection refused etc. -> Mongodb Dienst läuft nicht!!!! -> starten!!!
 */
class CaseClassesWithCirceCodecs extends CatsEffectSuite {

  def mongoClientRes:  Resource[IO, MongoClient[IO]] = {
    MongoClient.fromConnectionString[IO]("mongodb://localhost:27017")
  }

  def mongoDbRes(database : String):   Resource[IO, IO[GenericMongoDatabase[IO, Stream[IO, *]]]] = {
    mongoClientRes.map(client =>   client.getDatabase(database))
  }


 // @derive(decoder, encoder,  show)
  case class Address(city: String, country: String)

  implicit def decoderOf[T, P](implicit v: Validate[T, P], dt: Decoder[T]): Decoder[T Refined P] =
    dt.emap(refineV[P].apply[T](_))

  implicit def encoderOf[T, P](implicit et: Encoder[T]): Encoder[T Refined P] =
    et.contramap(_.value)

  implicit def showOf[T, P](implicit d: Show[T]): Show[T Refined P] =
    Show.show((rtp: T Refined P) => d.show(rtp.value))

  implicit def validateSizeN[N <: Int, R](implicit w: ValueOf[N]): Validate.Plain[R, Size[N]] =
    Validate.fromPredicate[R, Size[N]](
      _.toString.size == w.value,
      _ => s"Must have ${w.value} digits",
      Size[N](w.value)
    )


  type String10 = String Refined MaxSize[10]
  //implicit val dec =  decoderOf[String, Size[4]]
  //implicit val enc =  encoderOf[String, Size[4]]
  //implicit val show =  showOf[String, String4]
  object String10 extends RefinedTypeOps[String10, String]

  type Minus5To20 = Double Refined Interval.Closed[-5.0, 20.0]
  object Minus5To20 extends RefinedTypeOps[Minus5To20, Double]

  sealed trait  PersonEntry

  // @derive(decoder, encoder)//, show
  case class Person(firstName: String10, lastName: String , address: Address = Address("München", "GER"), registrationDate: Instant = Instant.now()) extends PersonEntry

  case class PersonG( firstName: String10,   lastName: String, balance :Minus5To20,  address: Address = Address("München", "GER"),  registrationDate: Instant = Instant.now()) extends PersonEntry

  case class Info(info : String)  extends PersonEntry

  //Compilefehler:
  //val pe = Person("Ernst", "Ha", 30.0, null)

  test("Person asJson") {
      val p = PersonG("Hans", "Hola", 15.0)
      println(p.asJson)
    }

  import TestData._

  test("Person decode") {
    println(decode[Person](personGJs))
    println(decode[PersonG](personGJs))
    println(decode[Info](infoJs))
  }


  test("Info asJson") {
    val p = Info( "Hola" )
    println(p.asJson)
  }


   object PersonEntry {
     def toEntry[FTP](e : Either[String, FTP])(cons : FTP => PersonEntry) =
       e.map(cons).fold(i =>(Info(i)), p => p)

    def apply(firstName: String, lastName: String, balance : Option[Double] = None) : PersonEntry = {
      toEntry(String10.from(firstName))(
        fn => balance.fold[PersonEntry](Person(fn, lastName))
                          ((bal:Double) => toEntry(Minus5To20.from(bal))(ba => PersonG(fn, lastName, ba) )  )
      )
    }
  }
   object PersonGEntry {
    def apply(firstName: String, lastName: String, balance :Double ) : PersonEntry = {
       (String10.from(firstName), Minus5To20.from( balance  )).mapN((fn, ba) => PersonG(fn, lastName, ba)) .fold(i =>(Info(i)), p => p)
    }
  }



  test("PersonEntry asJson ") {
    val p : PersonEntry  = Person("Hans", "Hola")
    println(p.asJson)
    val pg: PersonEntry  = PersonG("Hans", "Hola", 15.0)//als PersonEntry:{ "info": "unbekannter typ: PersonG(Hans,Hola,15.0,Address(München,GER),2023-03-29T08:29:16.068060400Z) }
    println(pg.asJson)
    val pgg: PersonG  = PersonG("Hans", "Hola", 15.0)// als PersonG
    println(pgg.asJson)
    val p1 : PersonEntry = Info("Except   ion")
    println(p1.asJson)

  }




  //eigener  myInstantDecoder
  def parseInstant(ops: => List[CursorOp])(d: String): Either[DecodingFailure, Instant] =
      Validated.fromTry(Try(Instant.parse(d))).leftMap(DecodingFailure.fromThrowable(_, ops)).toEither

  val myInstantDecoder: Decoder[Instant] = (c: HCursor) => c.as[String].flatMap(parseInstant(c.history))
  //standard instantDecoder either myInstantDecoder
  implicit val eInstantDecoder: Decoder[Instant] =
    mongo4cats.circe.instantDecoder.either(myInstantDecoder).map(_.merge)

  //eigener instantEncoder (ohne extra JsonObject $time...)
  implicit val instantEncoder: Encoder[Instant] =
    Encoder.encodeJson.contramap[Instant](i => Json.fromString(i.toString))

  val instantShow: Show[Instant] =Show[String].contramap[Instant](_.toString)


  test("PersonEntry DeCode ") {
      println(decode[PersonEntry](personGJs))
      println(decode[PersonG](personGJs))
      println(decode[PersonEntry](infoJs))
    }

  test("Person find") {
    mongoClientRes.use { client =>
      def useS(persStream: Stream[IO, PersonEntry]) = persStream.evalTap((pe: PersonEntry) => IO.println(pe)).map {
        case pg@PersonG(_, _, _, _, _) => s"eine PersonG ${pg.firstName}"
        case p@Person(_, _, _, _) => s"eine Person ${p.firstName}"
        case i@Info(_) => s"eine Info ${i.info}"
      }.evalTap(IO.println(_)).compile.drain

      for {
        db <- client.getDatabase("testdb")
        coll <- db.getCollectionWithCodec[PersonEntry]("people")
        allPers <- useS(coll.find.stream)
        // allPers <- coll.find.stream.compile.toList //
        // _ <- IO.println(allPers)
      } yield allPers

     }
  }



  test("PersonG mongo") {
    mongoClientRes.use { client =>
      for {
        db <- client.getDatabase("testdb")
    //    coll <- db.createCollection("peopleG")
        coll <- db.getCollectionWithCodec[PersonEntry]("peopleG")
          persons = 1.to(3).map(i => PersonEntry("Egon"*i, "Keil" +i+"berg"))
          personGs = 1.to(3).map(i => PersonEntry("Bibi"*i, "Bloggs" +i+"berg", Some(-10.0 + 10*i)))
          infos = 1.to(3).map(i => Info("Schitt Fehler"))
   //     _    <- coll.insertMany(persons ++ personGs ++ infos)
        somePers <- coll.find.filter(Filter.gt("balance", 10.0)).all
        _    <- IO.println(somePers)
        allPers <- coll.find.stream.compile.toList
        _    <- IO.println(allPers)
      } yield ()
    }
  }




  sealed trait PaymentMethod
  case class CreditCard(name: String, number: String, expiry: String, cvv: Int) extends PaymentMethod
  case class Paypal(email: String)                                              extends PaymentMethod


  test("PaymentMethod decode"){

  }



  case class Payment(
                            id: ObjectId,
                            amount: BigDecimal,
                            method: PaymentMethod,
                            date: Instant
                          )

  test("encode and decode case classes that extend sealed traits")  {
    val ts = Instant.parse("2020-01-01T00:00:00Z")
    val p1 = Payment(ObjectId(), BigDecimal(10), Paypal("foo@bar.com"), ts.plus(1, ChronoUnit.DAYS))
    val p2 = Payment(ObjectId(), BigDecimal(25), CreditCard("John Bloggs", "1234", "1021", 123), ts.plus(2, ChronoUnit.DAYS))

    //withEmbeddedMongoClient { client =>
    mongoClientRes.use { client =>
      val result = for {
        db       <- client.getDatabase("testdb")
        _        <- db.createCollection("payments")
        coll     <- db.getCollectionWithCodec[Payment]("payments")
        _        <- coll.insertMany(List(p1, p2))
        payments <- coll.find.filter(Filter.gt("date", ts)).all
        _    <- IO.println(payments)
      } yield payments

      result.map(_.toList == List(p1, p2))
    }
  }


  test("find Payment")  {
    val ts = Instant.parse("2020-01-01T00:00:00Z")

    mongoClientRes.use { client =>
      val result = for {
        db       <- client.getDatabase("testdb")
        coll     <- db.getCollectionWithCodec[Payment]("payments")
        payments <- coll.find.filter(Filter.lt("date", ts)).all
        _    <- IO.println(payments)
      } yield payments

      result.map(_.toList == List())
    }
  }



  object TestData {

    val personJs =
      """{
        |  "Person" : {
        |    "firstName" : "Hans",
        |    "lastName" : "Hola",
        |    "address" : {
        |      "city" : "München",
        |      "country" : "GER"
        |    },
        |    "registrationDate" : "2023-03-30T13:59:13.567031800Z"
        |  }
        |}"""
    val personGJs =
      """{
        |  "PersonG" : {
        |    "firstName" : "Hans",
        |    "lastName" : "Hola",
        |    "balance" : 15.0,
        |    "address" : {
        |      "city" : "München",
        |      "country" : "GER"
        |    },
        |    "registrationDate" : "2023-03-30T13:59:13.655351600Z"
        |  }
        |}
        |""".stripMargin
    val infoJs =
      """{
        |  "Info" : {
        |    "info" : "Except   ion"
        |  }
        |}
        |"""

    object CustomDeEncoder {
      /*Standard De/Encoder für "Oder-Typ": cl1, cl2,... extends trait: mit Zwischenknoten für Typ
      {
         "Person/PersonG/Info" : {
            "attr" : "value",
            ....
         }
      }

      //Eigener De/Encoder für "Oder-Typ": cl1, cl2,... extends trait: unmittelbares Json
      {
          "attr" : "value",
          ....
      }
      */

      //eigene De/Encoder: wirkt nur auf Objekt selber, d.h. ="Blatt-Mapper"

      //  implicit : damit es in .asJson(..) verwendet wird
      implicit val personEntryEncoder: Encoder[PersonEntry] =
        new Encoder[PersonEntry] {
          final def apply(xs: PersonEntry): Json = {
            xs match {
              case p@Person(_, _, _, _) => p.asJson
              case p@PersonG(_, _, _, _, _) => p.asJson
              case i@Info(_) => i.asJson
            }
          }
        }

      //eigener PersonEntry-Decoder mit direktem  Json {JJJ} (ohne { typ : {JJJ} }
      implicit def personEntryDecoder(implicit idec: Decoder[Info], pd: Decoder[Person], pgd: Decoder[PersonG]): Decoder[PersonEntry] =
        idec.either(pgd.either(pd).map(_.merge)).map(_.merge)


      val personJs =
        """{
          |  "firstName": "Bib3",
          |  "lastName": "Bloggs3berg",
          |  "address": {
          |    "city": "München",
          |    "country": "GER"
          |  },
          |  "registrationDate": {
          |    "$date": "2023-03-08T10:01:50.339Z"
          |  }
          |}""".stripMargin

      val personGJs =
        """{
          |  "firstName" : "Hans",
          |  "lastName" : "Hola",
          |  "balance" : 15.0,
          |  "address" : {
          |    "city" : "München",
          |    "country" : "GER"
          |  },
          |  "registrationDate" : "2023-03-27T16:21:26.409911500Z"
          |}""".stripMargin
      /*
      "registrationDate" : {
        "$date" : "2023-03-27T16:21:26.409911500Z"
      }

       */
      val infoJs =
        """{
          |  "info" : "DecodingFailure at .balance: Missing required field"
          |}""".stripMargin


    }



  }



}



