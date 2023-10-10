package Chapter10

import cats.data.{NonEmptyList, Validated}
import cats.implicits.{catsSyntaxTuple2Semigroupal, catsSyntaxValidatedId}

object Examples extends App {

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] = NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(error(s"Must be longer than $n characters"), str => str.length > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(error(s"Must be all alphanumeric characters"), str => str.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain the character $char"), str => str.contains(char))

  val splitEmail: Check[Errors, String, (String, String)] = {
    Check(x => x.split('@') match {
    case Array(name, domain) =>
      (name, domain).validNel[String]
    case _ =>
      "Must contain a single @ character".
        invalidNel[(String, String)]
  })
  }

  val checkLeft: Check[Errors, String, String] = Check(longerThan(0))
  val checkRight: Check[Errors, String, String] = Check(longerThan(3) and contains('.'))

  val joinEmail: Check[Errors, (String, String), String] = Check(el => el match {
    case (l, r) => (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)})

  val checkUserName: Check[Errors, String, String] = Check(longerThan(3) and alphanumeric)
  val checkEmail: Check[Errors, String, String] = splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  def createUser(username: String, email: String): Validated[Errors, User] =
    (checkUserName(username), checkEmail(email)).mapN(User)

  println(createUser("Noel", "noel@underscore.io"))
  println(createUser("", "dave@underscore@io"))
}
