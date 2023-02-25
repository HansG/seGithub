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
import cats.effect.{IO, IOApp}
import cats.implicits.toContravariantOps
import derevo.cats.{eqv, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import dev.profunktor.auth.jwt.JwtToken
import io.circe.{Decoder, Encoder, Json, JsonObject}
import io.circe.generic.auto._
import mongo4cats.client.MongoClient
import mongo4cats.circe._
import org.bson.codecs.configuration.{CodecRegistries, CodecRegistry}
import shop.domain.brand.BrandParam
import shop.domain.cart.Cart
import skunk.syntax.id

import java.time.Instant
import scala.util.{Success, Try}

object CaseClassesWithCirceCodecs extends IOApp.Simple {

  @derive(decoder, encoder,  show)
  final case class Address(city: String, country: String)
  @derive(decoder, encoder, show)
  final case class Person(firstName: String, lastName: String, address: Address, registrationDate: Instant)

//  object Instant {
// }
 // val dateTag = "$date"

  implicit val jsonEncoder: Encoder[Cart] =
    Encoder.forProduct1("items")(_.items)
  implicit val jsonDecoder: Decoder[Cart] =
    Decoder.forProduct1("items")(Cart.apply)


  val defaultInstant = Try(Instant.parse(("1900-01-01T00:00:00.000+00:00")))

  implicit val instantEncoder: Encoder[Instant] =
    Encoder.forProduct1("registrationDate")(i =>  i.toString)
  implicit val instantDecoder: Decoder[Instant] =
    Decoder.forProduct1("registrationDate") ((dateS:String) => Instant.parse(dateS.toString))
 //   Decoder[String].emapTry(dateTag => Try(Instant.parse(dateTag)).fold(_ => defaultInstant, i =>  Try(i) ))
  implicit val instantShow: Show[Instant] =Show[String].contramap[Instant](_.toString)


  override val run: IO[Unit] =
   //MongoClient.fromConnectionString[IO]("mongodb://localhost:27017").use { client =>
    MongoClient.fromConnectionString[IO]("mongodb://localhost:27017").map(client =>   client.getDatabase("testdb")).use { dbio =>
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
