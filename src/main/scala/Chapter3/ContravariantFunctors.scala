package Chapter3

object ContravariantFunctors extends App {
  trait Printable[A] {
    self =>
    def format(value: A): String

    // First wrong implementation
//    def contramap[B](func: B => A): Printable[B] = new Printable[B] {
//        def format(value: B): String = func(value).toString
//      }
    def contramap[B](func: B => A): Printable[B] = new Printable[B] {
      def format(value: B): String = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  implicit val stringPrintable: Printable[String] = new Printable[String] {
      def format(value: String): String = "\"" + value + "\""
    }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
      def format(value: Boolean): String = if(value) "yes" else "no"
    }

  println(format("hello"))
  // res3: String = "hello"

  println(format(true))
  // res4: String = yes

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit ev: Printable[A]): Printable[Box[A]] = ev.contramap[Box[A]](_.value)

  println(format(Box("hello world")))
  // res5: String = "hello world"
  println(format(Box(true)))
  // res6: String = yes
}
