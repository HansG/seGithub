package shop

import cats.effect.IO
import cats.{Foldable, Parallel, Show}

import java.util.UUID
import shop.domain.auth._
import shop.domain.brand._
import shop.domain.cart._
import shop.domain.category._
import shop.domain.checkout._
import shop.domain.item._
import shop.domain.order._
import shop.domain.payment.Payment
import shop.http.auth.users._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.string.ValidBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import shop.http.routes.BrandRoutes
import squants.market._
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

import scala.tools.nsc.doc.html.HtmlTags.I

object generators extends App {

  val nonEmptyStringGen: Gen[String] =
    Gen
      .chooseNum(21, 40)
      .flatMap { n =>
        Gen.buildableOfN[String, Char](n, Gen.alphaChar)
      }

  def nesGen[A](f: String => A): Gen[A] =
    nonEmptyStringGen.map(f)

  def idGen[A](f: UUID => A): Gen[A] =
    Gen.uuid.map(f)

  val brandIdGen: Gen[BrandId] =
    idGen(BrandId.apply)

  val brandNameGen: Gen[BrandName] =
    nesGen(BrandName.apply)

  val categoryIdGen: Gen[CategoryId] =
    idGen(CategoryId.apply)

  val categoryNameGen: Gen[CategoryName] =
    nesGen(CategoryName.apply)

  val itemIdGen: Gen[ItemId] =
    idGen(ItemId.apply)

  val itemNameGen: Gen[ItemName] =
    nesGen(ItemName.apply)

  val itemDescriptionGen: Gen[ItemDescription] =
    nesGen(ItemDescription.apply)

  val userIdGen: Gen[UserId] =
    idGen(UserId.apply)

  val orderIdGen: Gen[OrderId] =
    idGen(OrderId.apply)

  val paymentIdGen: Gen[PaymentId] =
    idGen(PaymentId.apply)

  val userNameGen: Gen[UserName] =
    nesGen(UserName.apply)

  val passwordGen: Gen[Password] =
    nesGen(Password.apply)

  val encryptedPasswordGen: Gen[EncryptedPassword] =
    nesGen(EncryptedPassword.apply)

  val quantityGen: Gen[Quantity] =
    Gen.posNum[Int].map(Quantity.apply)

  val moneyGen: Gen[Money] =
    Gen.posNum[Long].map(n => USD(BigDecimal(n)))

  val brandGen: Gen[Brand] =
    for {
      i <- brandIdGen
      n <- brandNameGen
    } yield Brand(i, n)

  val categoryGen: Gen[Category] =
    for {
      i <- categoryIdGen
      n <- categoryNameGen
    } yield Category(i, n)

  val itemGen: Gen[Item] =
    for {
      i <- itemIdGen
      n <- itemNameGen
      d <- itemDescriptionGen
      p <- moneyGen
      b <- brandGen
      c <- categoryGen
    } yield Item(i, n, d, p, b, c)

  val cartItemGen: Gen[CartItem] =
    for {
      i <- itemGen
      q <- quantityGen
    } yield CartItem(i, q)

  val cartTotalGen: Gen[CartTotal] =
    for {
      i <- Gen.nonEmptyListOf(cartItemGen)
      t <- moneyGen
    } yield CartTotal(i, t)

  val itemMapGen: Gen[(ItemId, Quantity)] =
    for {
      i <- itemIdGen
      q <- quantityGen
    } yield i -> q

  val cartGen: Gen[Cart] =
    Gen.nonEmptyMap(itemMapGen).map(Cart.apply)

  val cardNameGen: Gen[CardName] =
    Gen.stringOf(Gen.oneOf(('a' to 'z') ++ ('A' to 'Z'))).map { x =>
      CardName(Refined.unsafeApply(x))
    }

  private def sized(size: Int): Gen[Long] = {
    def go(s: Int, acc: String): Gen[Long] =
      Gen.oneOf(1 to 9).flatMap { n =>
        if (s == size) acc.toLong
        else go(s + 1, acc + n.toString)
      }

    go(0, "")
  }

  //XX
  object GenSum extends SimpleIOSuite  with Checkers{
    sealed trait A
    case class B(b: String) extends A
    case class C(c: String) extends A
    object A {
      implicit def toShow : Show[A with Product] =
    }
    val genS =  Gen.stringOf(Gen.oneOf(('a' to 'z') ++ ('A' to 'Z')))
    val genB =  genS.map (B(_))
    val genC =  genS.map (C(_))
    val genA = Gen.oneOf(genB, genC)


    def run = test("Show A's") {
      forall(genA) { a =>
        IO(println(a.toString))
        expect.same("A","A")
      }
    }
  }

  GenSum.run

  val cardGen: Gen[Card] =
    for {
      n <- cardNameGen
      u <- sized(16).map(x => CardNumber(Refined.unsafeApply(x)) )
      //u <- sized(16).map(x =>   CardNumber.from(x))//XX liefert Either -> MonadTransformer für Gen, Either...
      e <- sized(4).map(x => CardExpiration(Refined.unsafeApply(x.toString))) //.from(x.toString))
      c <- sized(3).map(x => CardCVV(Refined.unsafeApply(x.toInt)))
    } yield    Card(n, u, e, c)
   // } yield  Parallel.parMap4(na, nu, e, c)((na, nu, e, c) => Card(CardName(na), CardNumber(nu), CardExpiration(e), CardCVV(c)))

  // http routes generators

  val userGen: Gen[User] =
    for {
      i <- userIdGen
      n <- userNameGen
    } yield User(i, n)

  val adminUserGen: Gen[AdminUser] =
    userGen.map(AdminUser(_))

  val commonUserGen: Gen[CommonUser] =
    userGen.map(CommonUser(_))

  val paymentGen: Gen[Payment] =
    for {
      i <- userIdGen
      m <- moneyGen
      c <- cardGen
    } yield Payment(i, m, c)

  val brandParamGen: Gen[BrandParam] =
    arbitrary[NonEmptyString].map(BrandParam(_))

  val createItemParamGen: Gen[CreateItemParam] =
    for {
      n <- arbitrary[NonEmptyString].map(ItemNameParam(_))
      d <- arbitrary[NonEmptyString].map(ItemDescriptionParam(_))
      p <- arbitrary[String Refined ValidBigDecimal].map(PriceParam(_))
      b <- brandIdGen
      c <- categoryIdGen
    } yield CreateItemParam(n, d, p, b, c)

}
