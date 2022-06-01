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

  lazy val nonEmptyStringGen: Gen[String] =
    Gen
      .chooseNum(21, 40)
      .flatMap { n =>
        Gen.buildableOfN[String, Char](n, Gen.alphaChar)
      }

  def nesGen[A](f: String => A): Gen[A] =
    nonEmptyStringGen.map(f)

  def idGen[A](f: UUID => A): Gen[A] =
    Gen.uuid.map(f)

  lazy val brandIdGen: Gen[BrandId] =
    idGen(BrandId.apply)

  lazy val brandNameGen: Gen[BrandName] =
    nesGen(BrandName.apply)

  lazy val categoryIdGen: Gen[CategoryId] =
    idGen(CategoryId.apply)

  lazy val categoryNameGen: Gen[CategoryName] =
    nesGen(CategoryName.apply)

  lazy val itemIdGen: Gen[ItemId] =
    idGen(ItemId.apply)

  lazy val itemNameGen: Gen[ItemName] =
    nesGen(ItemName.apply)

  lazy val itemDescriptionGen: Gen[ItemDescription] =
    nesGen(ItemDescription.apply)

  lazy val userIdGen: Gen[UserId] =
    idGen(UserId.apply)

  lazy val orderIdGen: Gen[OrderId] =
    idGen(OrderId.apply)

  lazy val paymentIdGen: Gen[PaymentId] =
    idGen(PaymentId.apply)

  lazy val userNameGen: Gen[UserName] =
    nesGen(UserName.apply)

  lazy val passwordGen: Gen[Password] =
    nesGen(Password.apply)

  lazy val encryptedPasswordGen: Gen[EncryptedPassword] =
    nesGen(EncryptedPassword.apply)

  lazy val quantityGen: Gen[Quantity] =
    Gen.posNum[Int].map(Quantity.apply)

  lazy val moneyGen: Gen[Money] =
    Gen.posNum[Long].map(n => USD(BigDecimal(n)))

  lazy val brandGen: Gen[Brand] =
    for {
      i <- brandIdGen
      n <- brandNameGen
    } yield Brand(i, n)

  lazy val categoryGen: Gen[Category] =
    for {
      i <- categoryIdGen
      n <- categoryNameGen
    } yield Category(i, n)

  lazy val itemGen: Gen[Item] =
    for {
      i <- itemIdGen
      n <- itemNameGen
      d <- itemDescriptionGen
      p <- moneyGen
      b <- brandGen
      c <- categoryGen
    } yield Item(i, n, d, p, b, c)

  lazy val cartItemGen: Gen[CartItem] =
    for {
      i <- itemGen
      q <- quantityGen
    } yield CartItem(i, q)

  lazy val cartTotalGen: Gen[CartTotal] =
    for {
      i <- Gen.nonEmptyListOf(cartItemGen)
      t <- moneyGen
    } yield CartTotal(i, t)

  lazy val itemMapGen: Gen[(ItemId, Quantity)] =
    for {
      i <- itemIdGen
      q <- quantityGen
    } yield i -> q

  lazy val cartGen: Gen[Cart] =
    Gen.nonEmptyMap(itemMapGen).map(Cart.apply)

  lazy val cardNameGen: Gen[CardName] =
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


  lazy val cardGen: Gen[Card] =
    for {
      n <- cardNameGen
      u <- sized(16).map(x => CardNumber(Refined.unsafeApply(x)) )
      //u <- sized(16).map(x =>   CardNumber.from(x))//XX liefert Either -> MonadTransformer für Gen, Either...
      e <- sized(4).map(x => CardExpiration(Refined.unsafeApply(x.toString))) //.from(x.toString))
      c <- sized(3).map(x => CardCVV(Refined.unsafeApply(x.toInt)))
    } yield    Card(n, u, e, c)
   // } yield  Parallel.parMap4(na, nu, e, c)((na, nu, e, c) => Card(CardName(na), CardNumber(nu), CardExpiration(e), CardCVV(c)))

  // http routes generators

  lazy val userGen: Gen[User] =
    for {
      i <- userIdGen
      n <- userNameGen
    } yield User(i, n)

  lazy val adminUserGen: Gen[AdminUser] =
    userGen.map(AdminUser(_))

  lazy val commonUserGen: Gen[CommonUser] =
    userGen.map(CommonUser(_))

  lazy val paymentGen: Gen[Payment] =
    for {
      i <- userIdGen
      m <- moneyGen
      c <- cardGen
    } yield Payment(i, m, c)

  lazy val brandParamGen: Gen[BrandParam] =
    arbitrary[NonEmptyString].map(BrandParam(_))


  //2022XX
  lazy val createItemParamGen: Gen[CreateItemParam] =
    for {
      n <- arbitrary[NonEmptyString].map(ItemNameParam(_))
      d <- arbitrary[NonEmptyString].map(ItemDescriptionParam(_))
      p <- arbitrary[String Refined ValidBigDecimal].map(PriceParam(_))
      b <- brandIdGen
      c <- categoryIdGen
    } yield CreateItemParam(n, d, p, b, c)

}
