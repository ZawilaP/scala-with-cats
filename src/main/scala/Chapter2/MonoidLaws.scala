package Chapter2

object MonoidLaws {
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
  }

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

  def checkMonoidicalLaws[A](el1: A, el2: A, el3: A)(implicit m: Monoid[A]): Unit = {
    println(s"-- ${m.toString} passes: \n" +
      s"1. Associative law ${associativeLaw[A](el1, el2, el3)(m)} \n" +
      s"2. Identity law ${identityLaw[A](el1)(m) && identityLaw[A](el2)(m)}")
  }
}
