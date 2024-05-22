package shop.services

import shop.domain.ID
import shop.domain.brand._
import shop.domain.category._
import shop.domain.item._
import shop.effects.GenUUID
import shop.sql.codecs._
import cats.effect._
import cats.syntax.all._
import org.junit.Test
import org.typelevel.ci
import shapeless.Generic
import skunk.syntax.id
import squants.market.USD

import java.util.UUID
//import shapeless.{::, Generic}
import skunk.{*:, _}
import skunk.implicits._
import squants.market.Money

trait Items[F[_]] {
  def findAll: F[List[Item]]
  def findBy(brand: BrandName): F[List[Item]]
  def findById(itemId: ItemId): F[Option[Item]]
  def create(item: CreateItem): F[ItemId]
  def update(item: UpdateItem): F[Unit]
}

object Items {
  def make[F[_]: Concurrent: GenUUID](
      postgres: Resource[F, Session[F]]
  ): Items[F] =
    new Items[F] {
      import ItemSQL._

      // In the book we'll see how to retrieve results in chunks using stream or cursor
      def findAll: F[List[Item]] =
        postgres.use(_.execute(selectAll))

      def findBy(brand: BrandName): F[List[Item]] =
        postgres.use { session =>
          session.prepare(selectByBrand).flatMap { ps =>
            ps.stream(brand, 1024).compile.toList
          }
        }

      def findById(itemId: ItemId): F[Option[Item]] =
        postgres.use { session =>
          session.prepare(selectById).flatMap { ps =>
            ps.option(itemId)
          }
        }

      def create(item: CreateItem): F[ItemId] =
        postgres.use { session =>
          session.prepare(insertItem).flatMap { cmd =>
            ID.make[F, ItemId].flatMap { id =>
              cmd.execute((id, item)).as(id)
            }
          }
        }

      def update(item: UpdateItem): F[Unit] =
        postgres.use { session =>
          session.prepare(updateItem).flatMap { cmd =>
            cmd.execute(item).void
          }
        }
    }

}


class ShapeTest {

  case class Foo(i: Int, s: String, b: Boolean)

  val fooGen = Generic[Foo]

  val foo = Foo(23, "foo", true)

  @Test
  def testIt(): Unit =  {
    val l = fooGen.to(foo)
//    l should be( )
    val r = 13 :: l.tail
    val newFoo = fooGen.from(r)
//    newFoo.i should be( )
  }
}

private object ItemSQL {

  val decoder: Decoder[Item] =  
    (itemId *: itemName *: itemDesc *: money *: brandId *: brandName *: categoryId *: categoryName).map { //hier kein  *: EmptyTuple!!
      case i *: n *: d  *: p  *: bi  *: bn  *: ci  *: cn   *: EmptyTuple => //hier ein  *: EmptyTuple!!!
        Item(i, n, d, p, Brand(bi, bn), Category(ci, cn))
      case _  =>  null
    }

  val selectAll: Query[Void, Item] =
    sql"""
        SELECT i.uuid, i.name, i.description, i.price, b.uuid, b.name, c.uuid, c.name
        FROM items AS i
        INNER JOIN brands AS b ON i.brand_id = b.uuid
        INNER JOIN categories AS c ON i.category_id = c.uuid
       """.query(decoder)

  val selectByBrand: Query[BrandName, Item] =
    sql"""
        SELECT i.uuid, i.name, i.description, i.price, b.uuid, b.name, c.uuid, c.name
        FROM items AS i
        INNER JOIN brands AS b ON i.brand_id = b.uuid
        INNER JOIN categories AS c ON i.category_id = c.uuid
        WHERE b.name LIKE $brandName
       """.query(decoder)

  val selectById: Query[ItemId, Item] =
    sql"""
        SELECT i.uuid, i.name, i.description, i.price, b.uuid, b.name, c.uuid, c.name
        FROM items AS i
        INNER JOIN brands AS b ON i.brand_id = b.uuid
        INNER JOIN categories AS c ON i.category_id = c.uuid
        WHERE i.uuid = $itemId
       """.query(decoder)

  val insertItem: Command[ItemId *: CreateItem *: EmptyTuple] =
    sql"""
        INSERT INTO items
        VALUES ($itemId, $itemName, $itemDesc, $money, $brandId, $categoryId)
       """.command.contramap {
      case id *: i *: EmptyTuple =>
        id *: i.name *: i.description *: i.price *: i.brandId *: i.categoryId *: EmptyTuple
      case _ => null
    }

  val updateItem: Command[UpdateItem] =
    sql"""
        UPDATE items
        SET price = $money
        WHERE uuid = $itemId
       """.command.contramap[UpdateItem] {
      case i => i.price *: i.id *: EmptyTuple
    }

}
