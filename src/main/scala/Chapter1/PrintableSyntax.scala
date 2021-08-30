package Chapter1

import Chapter1.PrintableTypeClass.Printable

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit ev: Printable[A]): String = ev.format(value)

    def print(implicit ev: Printable[A]): Unit = println(format(ev))
  }
}
