package Chapter1

object PrintableTypeClass {

  trait Printable[A] {
    def format(value: A): String
  }

  object PrintableInstances {
    implicit val stringPrintable: Printable[String] = new Printable[String] {
      def format(value: String): String = value
    }

    implicit val intPrintable: Printable[Int] = new Printable[Int] {
      def format(value: Int): String = value.toString
    }
  }

  object Printable {
    def format[A](value: A)(implicit ev: Printable[A]): String = ev.format(value)

    def print[A](value: A)(implicit ev: Printable[A]): Unit = println(format(value))
  }

}
