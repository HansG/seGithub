
import mongo4cats.bson.{BsonValue, Document, ObjectId}

import java.time.Instant



val ts: Instant = Instant.now()
val id: ObjectId = ObjectId.gen

val doc1: Document = Document(
  "_id"            -> BsonValue.objectId(id),
  "null"           -> BsonValue.Null,
  "string"         -> BsonValue.string("str"),
  "int"            -> BsonValue.int(1),
  "boolean"        -> BsonValue.boolean(true),
  "double"         -> BsonValue.double(2.0),
  "int"            -> BsonValue.int(1),
  "long"           -> BsonValue.long(ts.toEpochMilli),
  "dateTime"       -> BsonValue.instant(ts),
  "array"          -> BsonValue.array(BsonValue.string("item1"), BsonValue.string("item2"), BsonValue.string("item3")),
  "nestedDocument" -> BsonValue.document(Document("field" -> BsonValue.string("nested")))
)

import mongo4cats.bson.syntax._
val doc2: Document = Document(
  "_id" := id,
  "null" := BsonValue.Null,
  "string" := "str",
  "int" := 1,
  "boolean" := true,
  "double" := 2.0,
  "int" := 1,
  "long" := ts.toEpochMilli,
  "dateTime" := ts,
  "array" := List("item1", "item2", "item3"),
  "nestedDocument" := Document("field" := "nested")
)

val updatedDoc1 = doc1.add("newField" -> BsonValue.string("string"))
val updatedDoc2 = doc2.add("newField" -> "string")
val updatedDoc3 = doc1 += ("anotherNewField" -> BsonValue.instant(ts))
val updatedDoc4 = doc2 += ("anotherNewField" := 1)

val json1: String = doc1.toJson
val json2: String = doc2.toJson

val stringField1: Option[BsonValue] = doc1.get("string")
val stringField2: Option[String] = doc1.getString("string")
val stringField3: Option[String] = doc1.getAs[String]("string")

val arrayField1: Option[BsonValue] = doc1.get("array")
val arrayField21: Option[BsonValue] = doc2.get("array")
val arrayField2: Option[List[BsonValue]] = doc1.getList("array")
val arrayField22: Option[List[BsonValue]] = doc2.getList("array")
val arrayField3: Option[List[String]] = doc1.getAs[List[String]]("array")
val arrayField23: Option[List[String]] = doc2.getAs[List[String]]("array")

val nestedField1: Option[BsonValue] = doc1.getNested("nestedDocument.field")
val nestedField2: Option[String] = doc2.getNestedAs[String]("nestedDocument.field")

doc1 == doc2
doc1.toJson
