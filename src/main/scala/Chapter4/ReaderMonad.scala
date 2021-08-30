package Chapter4

import cats.data.Reader

object ReaderMonad extends App {
  case class Db(
                 usernames: Map[Int, String],
                 passwords: Map[String, String]
               )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(Db => Db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(Db => Db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      username <- findUsername(userId)
      passwordExists <- username.map(name => checkPassword(name, password))
        .getOrElse(Reader[Db, Boolean](_ => false))
    } yield passwordExists
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

  println(checkLogin(1, "zerocool").run(db))
  println(!checkLogin(4, "davinci").run(db))
}
