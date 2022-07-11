package exa

import cats.Foldable.ops.toAllFoldableOps
import cats.Monad
import cats.Traverse.ops.toAllTraverseOps
import cats.conversions.all.autoWidenFunctor
import cats.data.{OptionT, Reader}
import cats.implicits.{toFoldableOps, toFunctorOps}
import com.sun.org.apache.xalan.internal.xsltc.compiler.sym
import geny.Generator.from
import natchez.Tags.db

import scala.Option.option2Iterable
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.mutable
import scala.meta.internal.Scaladoc.TagType.See

object CatsTry extends App {
  import cats.{ Eval, Id }
  import cats.syntax.applicative._ // for pure
  import cats.instances.vector._   // for Monoid
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.{ Await, Future }
  import scala.concurrent.duration._

  object Writer {
    import cats.data.Writer
    import cats.data.WriterT
    import cats.syntax.writer._ // for tell

    def slowly[A](body: => A) =
      try body
      finally Thread.sleep(100)

    def factorial(n: Int): Int = {
      val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
      println(s"fact $n $ans")
      ans
    }

    def factorial(n: BigInt): Eval[BigInt] =
      if (n == 1) {
        Eval.now(n)
      } else {
        factorial(n - 1).map(_ * n)
      }

    type Logged[A] = Writer[Vector[String], A]

    def factorialM(n: Int): Writer[String, Int] = slowly(
      if (n == 0) 1.writer("fact 0 = 1")
      else
        factorialM(n - 1).map(f => n * f).mapBoth((log, f) => (log + (s"\nfact ${n} = $f"), f))
    )

    val writer1: Logged[Int] = for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield a + b

    def factorial1(n: BigInt): Logged[BigInt] =
      for {
        ans <- if (n == 0) {
          BigInt(1).pure[Logged]
        } else {
          slowly(factorial1(n - 1).map(_ * n))
        }
        _ <- Vector(s"fact $n $ans").tell
      } yield ans

    implicit def orderingf[A](implicit ord: Ordering[A]): Ordering[Logged[A]] = Ordering.by(_.value)

    val logs = Await.result(
      Future
        .sequence(
          Vector(
            Future(factorial1(50)),
            Future(factorial1(40))
          )
        )
        .map(_.map(_.mapWritten(_.mkString(", ")))),
      5.seconds
    )
    // )).map(_.map(_.written.mkString(", ")  )), 5.seconds)

    println(logs.mkString("\nNext:\n"))

  }

  object ReaderTry {

    final case class Db(usernames: Map[Int, String], passwords: Map[String, String])

    type DbReader[A] = Reader[Db, A]

    def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))

    def checkPassword(username: String, password: String): DbReader[Boolean] =
      Reader(db => db.passwords.get(username).fold(false)(_ == password))

    def checkLogin(userId: Int, password: String): DbReader[Boolean] = findUsername(userId).flatMap { userNameOpt =>
      userNameOpt.fold(Reader((_: Db) => false))((userName: String) => checkPassword(userName, password))
    }

    def checkLogin1(userId: Int, password: String): DbReader[Boolean] =
      for {
        username <- findUsername(userId)
        passwordOk <- username
          .map { username =>
            checkPassword(username, password)
          }
          .getOrElse {
            false.pure[DbReader]
          }
      } yield passwordOk

    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade"  -> "zerocool",
      "kate"  -> "acidburn",
      "margo" -> "secret"
    )
    val db = Db(users, passwords)

    println(checkLogin(1, "zerocool").run(db))
    println(checkLogin(4, "davinci").run(db))

  }

  object StateTry {

    import cats.data.State
    import State._

    val program: State[Int, (Int, Int, Int)] = for {
      a <- get[Int]
      _ <- set[Int](a + 1)
      b <- get[Int]
      _ <- modify[Int](_ + 1)
      c <- inspect[Int, Int](_ * 1000)
    } yield (a, b, c)

    val (state, result) = program.run(1).value
  }

  object Calc {

    import cats.Eval
    import cats.data.State

    import scala.collection.mutable.Stack

    def op(sym: String): Option[(Int, Int) => Int] =
      sym match {
        case "+" => Some(_ + _)
        case "-" => Some(_ - _)
        case "*" => Some(_ * _)
        case "/" => Some(_ / _)
        case _   => None
      }

    type CS = State[Stack[Int], Int]

    def step(sym: String): CS = State[Stack[Int], Int] { (stk: Stack[Int]) =>
      val stk1 = op(sym)
        .map(op => op(stk.pop(), stk.pop()))
        .map(re => stk.push(re))
        .getOrElse(stk.push(sym.toInt))
      (stk1, stk1.top)
    }

    def calc(syms: String) =
      syms
        .split(' ')
        .foldLeft {
          State[Stack[Int], Int](_ => (mutable.Stack(), 0))
        } { (st, sym) =>
          st.flatMap(_ => step(sym))
        }
  }

  object TreeMonad {

    sealed trait Tree[+A]
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A)                        extends Tree[A]

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)
    def leaf[A](value: A): Tree[A] =
      Leaf(value)

    import cats.Monad
    implicit val treeMonad = new Monad[Tree] {
      def pure[A](value: A): Tree[A] =
        Leaf(value)
      def flatMap[A, B](tree: Tree[A])(func: A => Tree[B]): Tree[B] =
        tree match {
          case Branch(l, r) =>
            Branch(flatMap(l)(func), flatMap(r)(func))
          case Leaf(value) =>
            func(value)
        }
      //tailRec siehe Scala with Cats S.128
      def tailRecM[A, B](a: A)(func: A => Tree[Either[A, B]]): Tree[B] =
        flatMap(func(a)) {
          case Left(value) =>
            tailRecM(value)(func)
          case Right(value) =>
            Leaf(value)
        }
    }


    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatMap
    branch(leaf(100), leaf(200)).
      flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

  }


  object TailRecTry {

    import cats.syntax.flatMap._ // For flatMap
    def retry[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
      f(start).flatMap{ a =>
        retry(a)(f)
      }


    def retryTailRecM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
      Monad[F].tailRecM(start){ a =>
        f(a).map(a2 => Left(a2))
      }

    import cats.syntax.monad._ // for iterateWhileM
    def retryM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
      start.iterateWhileM(f)(a => true)


    import cats.syntax.applicative._ // for pure
    // Hypothetical example. This won't actually compile:
    def withInOpt[M[_]: Monad ] = {
      type Composed[A] = M[Option[A]]
      new Monad[Composed] {
        override def pure[A](a: A): Composed[A] = a.pure[Option].pure[M]
        override def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] =
              Monad[M].flatMap(fa)((opt:Option[A]) =>  opt.map(f).getOrElse(None.pure[M]))

        override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
      }
    }
    //funktioniert nicht !!!!  def withOutOpt[M[_]: Monad ] = ???
    //type ListOption[A] = List[Option[A]]
   // implicit  val lm: Monad[ListOption] = withInOpt[List]
    import cats.instances.list._ // for Monad
    type ListOption[A] =  OptionT[List, A]
    val result1: ListOption[Int] = OptionT(List(Option(10)))
    // result1: ListOption[Int] = OptionT(List(Some(10)))
    val result2: ListOption[Int] = 32.pure[ListOption]

    val i3 = result1.flatMap( i => result2.map(i2 => i2 + i) )



  }



}
