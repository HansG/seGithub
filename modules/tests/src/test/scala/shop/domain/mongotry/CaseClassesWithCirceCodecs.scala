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
import io.circe.{Decoder, Encoder, HCursor, Json, JsonObject}
import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import io.circe.parser.decode
import io.circe.syntax._
import mongo4cats.bson.ObjectId
import mongo4cats.client.MongoClient
import mongo4cats.circe._
import mongo4cats.operations.Filter
import munit.CatsEffectSuite
import org.bson.codecs.configuration.{CodecRegistries, CodecRegistry}
import shop.domain.cart.Cart
import skunk.syntax.id

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.language.postfixOps
import scala.util.{Success, Try}


/*
MongoSocketOpenException: Exception opening socket  connection refused etc. -> Mongodb Dienst läuft nicht!!!! -> starten!!!
 */
class CaseClassesWithCirceCodecs extends CatsEffectSuite {

  def mongoClientRes:  Resource[IO, MongoClient[IO]] = {
    MongoClient.fromConnectionString[IO]("mongodb://localhost:27017")
  }




 // @derive(decoder, encoder,  show)
  case class Address(city: String, country: String)

  implicit def decoderOf[T, P](implicit v: Validate[T, P], d: Decoder[T]): Decoder[T Refined P] =
    d.emap(refineV[P].apply[T](_))

  implicit def encoderOf[T, P](implicit d: Encoder[T]): Encoder[T Refined P] =
    d.contramap(_.value)

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


 // @derive(decoder, encoder)//, show
  case class Person(firstName: String10, lastName: String, balance :Minus5To20, address: Address = Address("München", "GER"), registrationDate: Instant = Instant.now())


    test("Person Codec") {
      val p = Person("Hans", "Hola", 15.0)
      println(p.asJson)
    }

  test("Person Codec2") {

    val js =
            """{
               |  "firstName" : "Hans",
               |  "lastName" : "Hola",
               |  "balance" : 15.0,
               |  "address" : {
               |    "city" : "München",
               |    "country" : "GER"
               |  },
               |  "registrationDate" : {
               |    "$date" : "2023-03-27T16:21:26.409911500Z"
               |  }
               |}""".stripMargin
    println(decode[Person](js))
    val jsf =
             """{
                |  "text" : "DecodingFailure at .balance: Missing required field",
                |}""".stripMargin

  }

  case class Meldung(text : String)

  case class PersonEntry(person : Either[String, Person])

   object PersonEntry {
    def apply(firstName: String, lastName: String, balance :Double ) : PersonEntry = {
      val pers = (String10.from(firstName), Minus5To20.from( balance  )).mapN((fn, ba) => Person(fn, lastName, ba))
      PersonEntry(pers)
    }
  }



  //Compilefehler:
  //val pe = Person("Ernst", "Ha", null, null)


  val defaultInstant = Try(Instant.parse(("1900-01-01T00:00:00.000+00:00")))


   val personEntryEncoder: Encoder[PersonEntry] =
    new Encoder[PersonEntry] {
      final def apply(xs: PersonEntry): Json = {
        xs.person.fold(
          fehler => Json.obj("fehler" -> Json.fromString(fehler) ),
          person => person.asJson
        )
      }
    }

 /*   Encoder.forProduct1("person")(i =>  i.person.fold(t => JsonObject("fehler", t), p => p.asJson))
  implicit val instantDecoder: Decoder[PersonEntry] =
    Decoder. ((personS:String) => if(personS.contains("fehler")) PersonEntry(Left(personS)) else Json.fromString(personS).as[Person].fold(decFail => PersonEntry(Left(decFail.getMessage())), pers => pers) )
  //   Decoder[String].emapTry(dateTag => Try(Instant.parse(dateTag)).fold(_ => defaultInstant, i =>  Try(i) ))
  implicit val decodeFoo: Decoder[PersonEntry] = new Decoder[PersonEntry] {
    final def apply(c: HCursor): Decoder.Result[PersonEntry] =
      for {
        foo <- c.downField("foo").as[String]
        bar <- c.downField("bar").as[Int]
      } yield {
        new PersonEntry(foo, bar)
      }
  }
  */


  implicit val instantShow: Show[Instant] =Show[String].contramap[Instant](_.toString)

  test("Person Codec") {
    //MongoClient.fromConnectionString[IO]("mongodb://localhost:27017").use { client =>
    mongoClientRes.map(client =>   client.getDatabase("testdb")).use { dbio =>
      for {
        //  db   <- client.getDatabase("testdb")
        db   <- dbio
        coll <- db.getCollectionWithCodec[PersonEntry]("persons")
        personEs = 1.to(4).map(i =>
                 // Person(String10("Pet"*i), "Bloggs" +i+"berg", Minus5To20(-10.0 + 10*i)))
               //   _    <- coll.insertMany(persons)
          PersonEntry("Pet"*i, "Bloggs" +i+"berg", -10.0 + 10*i))
        _    <- coll.insertMany(personEs)
        somePers <- coll.find.filter(Filter.gt("balance", 10.0)).all
        _    <- IO.println(somePers)
        allPers <- coll.find.stream.compile.toList
        _    <- IO.println(allPers)
      } yield ()
    }
  }

  test("Person find") {
    //MongoClient.fromConnectionString[IO]("mongodb://localhost:27017").use { client =>
    mongoClientRes.map(client =>   client.getDatabase("testdb")).use { dbio =>
      for {
        //  db   <- client.getDatabase("testdb")
        db   <- dbio
        coll <- db.getCollectionWithCodec[PersonEntry]("persons")
        somePers <- coll.find.filter(Filter.gt("person.Right.value.balance", 10.0)).all
        _    <- IO.println(somePers)
        allPers <- coll.find.stream.compile.toList
        _    <- IO.println(allPers)
      } yield ()
    }
  }



  sealed trait PaymentMethod
  case class CreditCard(name: String, number: String, expiry: String, cvv: Int) extends PaymentMethod
  case class Paypal(email: String)                                              extends PaymentMethod

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






}
