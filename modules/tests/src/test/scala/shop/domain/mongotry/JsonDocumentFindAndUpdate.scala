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

import cats.effect.{IO, IOApp}
import com.mongodb.client.model.TextSearchOptions
import mongo4cats.client.MongoClient
import mongo4cats.operations.{Filter, Update}
import mongo4cats.bson.Document
import munit.CatsEffectSuite

class JsonDocumentFindAndUpdate  extends CatsEffectSuite  {

  val json =
    """{
      |"firstName": "John",
      |"lastName": "Bloggs",
      |"dob": "1970-01-01"
      |}""".stripMargin

  val filterQuery = Filter.eq("lastName", "Bloggs") || Filter.eq("firstName", "John")

  val updateQuery = Update
    .set("dob", "2020-01-01")
    .rename("firstName", "name")
    .currentDate("updatedAt")
    .unset("lastName")

  test("insert & update doc") {
    MongoClient.fromConnectionString[IO]("mongodb://localhost:27017").use { client =>
      for {
        db      <- client.getDatabase("testdb")
        coll    <- db.getCollection("jsoncoll")
        _       <- coll.insertOne(Document.parse(json))
        old     <- coll.findOneAndUpdate(filterQuery, updateQuery)
        updated <- coll.find.first
     //   docs1 <- coll.find(Filter("""{name: {$regex : /string/i}}""")).stream.compile.toList
        _       <- IO.println(s"Retrieved documents:\nold: ${old.get.toJson}\nupdated: ${updated.get.toJson}")
      } yield ()
    }
  }


 test("find doc") {
    MongoClient.fromConnectionString[IO]("mongodb://localhost:27017").use { client =>
      for {
        db      <- client.getDatabase("testdb")
        coll    <- db.getCollection("jsoncoll")
        updated <- coll.find.first
        _       <- IO.println(s"Retrieved documents:\nupdated: ${updated.get.toJson}")
        _       <- IO.println(s"updated: ${updated.get.get("name").get.asString}")//updatedAtasDocument.get.toJson
      } yield ()
    }
  }
}
