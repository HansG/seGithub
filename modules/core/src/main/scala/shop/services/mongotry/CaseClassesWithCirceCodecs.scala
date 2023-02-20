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

package shop.services.mongotry

import cats.effect.{IO, IOApp}
import derevo.cats.{eqv, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.generic.auto._
import mongo4cats.client.MongoClient
import mongo4cats.circe._
import org.bson.codecs.configuration.CodecRegistry

import java.time.Instant

object CaseClassesWithCirceCodecs extends IOApp.Simple {

  @derive(decoder, encoder, eqv, show)
  final case class Address(city: String, country: String)
  @derive(decoder, encoder, eqv, show)
  final case class Person(firstName: String, lastName: String, address: Address, registrationDate: Instant)

  override val run: IO[Unit] =
    MongoClient.fromConnectionString[IO]("mongodb://localhost:27017").use { client =>
      for {
        db   <- client.getDatabase("testdb")
    //    coll <- db.getCollectionWithCodec[Person]("people")
        coll <- db.getCollection[Person]("people", CodecRegistry)
        person = Person("John", "Bloggs", Address("New-York", "USA"), Instant.now())
        _    <- coll.insertOne(person)
        docs <- coll.find.stream.compile.toList
        _    <- IO.println(docs)
      } yield ()
    }
}
