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

import de.flapdoodle.os.common.matcher.Matchers
import io.circe.generic.auto._
import mongo4cats.bson.{Document, ObjectId}
import mongo4cats.circe._
import mongo4cats.bson.syntax._
import mongo4cats.operations.Filter
import munit.CatsEffectSuite
import org.scalatest.matchers.must.Matchers.{convertToAnyMustWrapper, have}
import shop.services.StartPostgres.mongoClientRes

import java.time.Instant

class DocumentsWithCirce extends CatsEffectSuite  {//with Matchers

  final case class MyClass(
      _id: ObjectId,
      dateField: Instant,
      stringField: String,
      intField: Int,
      longField: Long,
      arrayField: List[String],
      optionField: Option[String]
  )

  val myClass = MyClass(
    _id = ObjectId.gen,
    dateField = Instant.now(),
    stringField = "string",
    intField = 1,
    longField = 1660999000L,
    arrayField = List("item1", "item2"),
    optionField = None
  )

  val doc = Document("_id" := ObjectId.gen, "myClasses" := List(myClass))

  val retrievedMyClasses = doc.getAs[List[MyClass]]("myClasses")

  println(doc.toJson)
  println(retrievedMyClasses)

  def categories(n: Int): Vector[Document] = (0 until n).map(i => Document("_id" := ObjectId.gen, "name" := s"cat-$i")).toVector

  test("stream with filter"){
    mongoClientRes.use { client =>
      val result = for {
         db   <- client.getDatabase("testdb")
        coll <- db.getCollection("coll")
        _ <- coll.insertMany(categories(50000))
        res <- coll.find.filter(Filter.regex("name", "cat-(1|3|5).*")).stream.compile.toList
      } yield res

      result.map(_ must have size 23333)
    }
  }

}
