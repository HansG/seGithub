package exa

import cats._
import cats.data._
import cats.Monoid
import cats.instances.option._
import cats.instances.bigInt._
import cats.instances.int._

import cats.arrow.FunctionK
import cats.{Id, ~>}
import scala.collection.mutable
import cats.free.Free
import cats.free.Free.liftF

object FreeMonadDPRecursive extends App {
  sealed trait ArrayA[A]

  case class Put[T](idx: Int, value: T) extends ArrayA[Unit]

  case class Get[T](idx: Int) extends ArrayA[Option[T]]

  type ArrayStore[A] = Free[ArrayA, A]

  // Put returns nothing (i.e. Unit).
  def put[T](idx: Int, value: T): ArrayStore[Unit] =
    liftF[ArrayA, Unit](Put[T](idx, value))

  // Get returns a T value.
  def get[T](idx: Int): ArrayStore[Option[T]] =
    liftF[ArrayA, Option[T]](Get[T](idx))

  def cdRecursive(i: Int): ArrayStore[Option[BigInt]] =
    for {
      v <- get[BigInt](i)
      iMinusOne <- if (v == Option(-1)) cdRecursive(i - 1) else get[BigInt](2)
      iMinusTwo <- if (v == Option(-1)) cdRecursive(i - 2) else get[BigInt](1)
      _ <- if (v == Option(BigInt(-1)))
      // FIXME: Use Monoid Instance for BigInt with combine = multiplication
      // and remove Option.get
        put[BigInt](i, (i - 1) * (Monoid[Option[BigInt]].combine(iMinusOne, iMinusTwo).get))
      else put[BigInt](1, 0) // This is a just a dummy put, not needed
      newV <- get[BigInt](i)
    } yield newV

  // There are flaws here
  def interpreter(n: Int): ArrayA ~> Id =
    new (ArrayA ~> Id) {

      val arr = new Array[Any](n + 1)
      arr(1) = BigInt(0)
      arr(2) = BigInt(1)
      (3 to n) map (i => arr(i) = -1)

      def apply[A](fa: ArrayA[A]): Id[A] =
        fa match {
          case Put(key, value) =>
            // println(s"put($key, $value)")
            arr(key) = value
            ()
          case Get(key) =>
            // println(s" -- get($key) -- ")
            Option(arr(key).asInstanceOf[A])
        }
    }

  def execRecursive(n: Int) = {
    val result: Option[BigInt] = cdRecursive(n).foldMap(interpreter(n))
    println(result)
  }
}