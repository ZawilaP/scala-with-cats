package Chapter3

object Codec extends App {

  trait Codec[A] {
    self =>

    def encode(value: A): String

    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      def encode(value: B): String = self.encode(enc(value))

      def decode(value: String): B = dec(self.decode(value))
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value

      def decode(value: String): String = value
    }

  implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)
  implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)

  implicit val doubleCodec: Codec[Double] = new Codec[Double] {
    def encode(value: Double): String = value.toString

    def decode(value: String): Double = value.toDouble
  }

  println(encode(123.4))
  println(encode(123.4).getClass)
  // res0: String = 123.4
  println(decode[Double]("123.4"))
  println(decode[Double]("123.4").getClass)

  // res1: Double = 123.4

  case class Box[A](value: A)

  implicit def boxCodec[A](implicit ev: Codec[A]): Codec[Box[A]] = new Codec[Box[A]] {
    def encode(value: Box[A]): String = ev.encode(value.value)

    def decode(value: String): Box[A] = Box(ev.decode(value))
  }

  println(encode(Box(123.4)))
  println(encode(Box(123.4)).getClass)
  // res2: String = 123.4
  println(decode[Box[Double]]("123.4"))
  println(decode[Box[Double]]("123.4").getClass)
}
