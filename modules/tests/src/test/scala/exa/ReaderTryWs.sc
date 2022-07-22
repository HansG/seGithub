import cats.data.Reader

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



}

import ReaderTry._
println(checkLogin(1, "zerocool").run(db))
println(checkLogin(4, "davinci").run(db))
