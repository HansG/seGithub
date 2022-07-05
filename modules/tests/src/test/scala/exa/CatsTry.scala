package exa

import cats.data.Reader
import natchez.Tags.db

import scala.meta.internal.Scaladoc.TagType.See


object CatsTry extends App{
  import cats.{Eval, Id}
  import cats.syntax.applicative._ // for pure
  import cats.instances.vector._ // for Monoid
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.{Await, Future}
  import scala.concurrent.duration._

  object Writer  {
    import cats.data.Writer
    import cats.data.WriterT
    import cats.syntax.writer._ // for tell



    def slowly[A](body: => A) =
      try body finally Thread.sleep(100)

    def factorial(n: Int): Int = {
      val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
      println(s"fact $n $ans")
      ans
    }


    def factorial(n: BigInt): Eval[BigInt] =
      if(n == 1) {
        Eval.now(n)
      } else {
        factorial(n - 1).map(_ * n)
      }

    type Logged[A] = Writer[Vector[String], A]


    def factorialM(n: Int): Writer[ String , Int] = slowly(
      if(n == 0)  1.writer("fact 0 = 1")
      else
        factorialM(n - 1).map(f => n*f).mapBoth( (log, f) => (log + (s"\nfact ${n} = $f"), f))
    )



    val writer1: Logged[Int] = for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield a + b

    def factorial1(n: BigInt) : Logged[BigInt] =
      for  {
        ans <- if(n == 0) {
                BigInt(1).pure[Logged]
              } else {
                slowly(factorial1(  n - 1  ).map(_ *  n ))
              }
        _ <- Vector(s"fact $n $ans").tell
      } yield ans


    implicit def orderingf[A](implicit ord: Ordering[A]): Ordering[Logged[A]] = Ordering.by(_.value)

    val logs = Await.result(Future.sequence(Vector(
      Future(factorial1(50)),
      Future(factorial1(40))
    )).map(_.map(_.mapWritten( _.mkString(", "))  )), 5.seconds)
    // )).map(_.map(_.written.mkString(", ")  )), 5.seconds)

    println(logs.mkString("\nNext:\n"))


  }

  object ReaderTry {

    final case class Db( usernames: Map[Int, String],
                         passwords: Map[String, String] )

    type DbReader[A] = Reader[Db, A]

    def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))


    def checkPassword( username: String,
                       password: String): DbReader[Boolean] = Reader( db => db.passwords.get(username).fold(false)(_ == password) )

    def checkLogin( userId: Int,
                    password: String): DbReader[Boolean] = findUsername(userId).flatMap { userNameOpt =>
      userNameOpt.fold(Reader((_: Db) => false))((userName: String) => checkPassword(userName, password))
    }


    def checkLogin1( userId: Int,
                    password: String): DbReader[Boolean] =
      for {
        username <- findUsername(userId)
        passwordOk <- username.map { username =>
          checkPassword(username, password)
        }.getOrElse {
          false.pure[DbReader]
        }
      } yield passwordOk


    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
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





 }
