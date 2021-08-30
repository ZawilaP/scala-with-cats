package Chapter1

import Chapter1.CatPrintable.Cat
import cats.Eq
import cats.implicits.catsSyntaxEq
import cats.instances.int._
import cats.instances.option._
import cats.instances.string._

object CatEquality extends App {

  implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) => {
    val nameEq = cat1.name === cat2.name
    val ageEq = cat1.age === cat2.age
    val colorEq = cat1.color === cat2.color

    nameEq && ageEq && colorEq
  }}

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(cat1 === cat2)
  println(cat1 =!= cat2)
  println(optionCat1 === optionCat2)
  println(optionCat1 =!= optionCat2)
}
