package Chapter6

import cats.data.Validated
import cats.syntax.either._
import cats.syntax.apply._

object Validated extends App {
  case class User(name: String, age: Int)

  def readName(args: Map[String, String]): Either[List[String], String] =
    args.getValue("name").flatMap(_.nonBlank("name"))

  def readAge(args: Map[String, String]): Either[List[String], Int] =
    args.getValue("age")
      .flatMap(_.nonBlank("age"))
      .flatMap(_.parseInt("age"))
      .flatMap(_.nonNegative("age"))

  def readUser(args: Map[String, String]): Validated[List[String], User] =
    (
      readName(args).toValidated,
      readAge(args).toValidated
    ).mapN(User.apply)

  implicit class MapOps(map: Map[String, String]) {
    def getValue(entry: String): Either[List[String], String] = map.get(entry).toRight(List(s"$entry field must be specified"))
  }

  implicit class StringOps(str: String) {
    def parseInt(entry: String): Either[List[String], Int] = Either.catchOnly[NumberFormatException](str.toInt).leftMap(_ => List(s"$entry must be an integer"))

    def nonBlank(entry: String): Either[List[String], String] = str.nonEmpty match {
      case false => Left(List(s"$entry cannot be blank"))
      case true => Right(str)
    }
  }

  implicit class IntOps(int: Int) {
    def nonNegative(entry: String): Either[List[String], Int] = int >= 0 match {
      case true => Right(int)
      case false => Left(List(s"$entry must be non-negative"))
    }
  }

  println(readName(Map("name" -> "Dade Murphy")))
  // res41: FailFast[String] = Right(Dade Murphy)
  println(readName(Map("name" -> "")))
  // res42: FailFast[String] = Left(List(name cannot be blank))
  println(readName(Map()))
  // res43: FailFast[String] = Left(List(name field not specified))
  println(readAge(Map("age" -> "11")))
  // res44: FailFast[Int] = Right(11)
  println(readAge(Map("age" -> "-1")))
  // res45: FailFast[Int] = Left(List(age must be non-negative))
  println(readAge(Map()))

  println(readUser(Map("name" -> "Dave", "age" -> "37")))
  println(readUser(Map("age" -> "-1")))
}
