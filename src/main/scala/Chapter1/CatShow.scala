package Chapter1

import Chapter1.CatPrintable.Cat
import cats._
import cats.implicits._

object CatShow extends App {
 implicit val catShow: Show[Cat] = Show.show(cat => s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat")

  println(Cat(name = "JT", age = 420, color = "blazed").show)
}
