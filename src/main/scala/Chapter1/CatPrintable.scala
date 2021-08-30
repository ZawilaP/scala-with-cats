package Chapter1

import Chapter1.PrintableTypeClass.Printable
import Chapter1.PrintableTypeClass.PrintableInstances._
import Chapter1.PrintableSyntax.PrintableOps

object CatPrintable extends App {
  final case class Cat(name: String, age: Int, color: String)

  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    def format(value: Cat): String = s"${Printable.format(value.name)} is a ${Printable.format(value.age)} year-old ${Printable.format(value.color)} cat"
  }

  val cat = Cat(name = "Kostek", age = 69, color = "pink")

  Printable.print(cat)

//  new PrintableOps[Cat](Cat(name = "JT", age = 420, color = "blazed")).print
  Cat(name = "JT", age = 420, color = "blazed").print
}
