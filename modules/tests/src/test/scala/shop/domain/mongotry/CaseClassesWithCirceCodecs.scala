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
import cats.effect.{IO, IOApp, Resource}
import cats.implicits.toContravariantOps
import derevo.cats.{eqv, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.{Decoder, Encoder, Json, JsonObject}
import io.circe.generic.auto._
import mongo4cats.bson.ObjectId
import mongo4cats.client.MongoClient
import mongo4cats.circe._
import mongo4cats.operations.Filter
import munit.CatsEffectSuite
import org.bson.codecs.configuration.{CodecRegistries, CodecRegistry}
import shop.domain.cart.Cart

import java.time.Instant
import java.time.temporal.ChronoUnit
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
  //@derive(decoder, encoder, show)
  case class Person(firstName: String, lastName: String, address: Address, registrationDate: Instant)

//  object Instant {
// }
 // val dateTag = "$date"

  implicit val jsonEncoder: Encoder[Cart] =
    Encoder.forProduct1("items")(_.items)
  implicit val jsonDecoder: Decoder[Cart] =
    Decoder.forProduct1("items")(Cart.apply)


  val defaultInstant = Try(Instant.parse(("1900-01-01T00:00:00.000+00:00")))

  /*
  implicit val instantEncoder: Encoder[Instant] =
    Encoder.forProduct1("registrationDate")(i =>  i.toString)
  implicit val instantDecoder: Decoder[Instant] =
    Decoder.forProduct1("registrationDate") ((dateS:String) => Instant.parse(dateS.toString))
 //   Decoder[String].emapTry(dateTag => Try(Instant.parse(dateTag)).fold(_ => defaultInstant, i =>  Try(i) ))
  implicit val instantShow: Show[Instant] =Show[String].contramap[Instant](_.toString)
*/

  test("Person Codec") {
    //MongoClient.fromConnectionString[IO]("mongodb://localhost:27017").use { client =>
    mongoClientRes.map(client =>   client.getDatabase("testdb")).use { dbio =>
      for {
        //  db   <- client.getDatabase("testdb")
        db   <- dbio
        coll <- db.getCollectionWithCodec[Person]("people")
        //        persons = 1.to(5).map( i => Person("Bib"+i, "Bloggs" +i+"berg", Address("München", "GER"), Instant.now()))
        //        _    <- coll.insertMany(persons)
        docs <- coll.find.stream.compile.toList
        _    <- IO.println(docs)
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
        db       <- client.getDatabase("test")
        _        <- db.createCollection("payments")
        coll     <- db.getCollectionWithCodec[Payment]("payments")
        _        <- coll.insertMany(List(p1, p2))
        payments <- coll.find.filter(Filter.gt("date", ts)).all
      } yield payments

      result.map(_.toList == List(p1, p2))
    }
  }






}
