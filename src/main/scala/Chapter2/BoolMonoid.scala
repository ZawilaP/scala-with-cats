package Chapter2

import Chapter2.MonoidLaws.{Monoid, checkMonoidicalLaws}

object BoolMonoid extends App {
  val boolAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = true

    def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  val boolOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = false

    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  val boolEitherMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = false

    def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
  }

  val boolNorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = true

    def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)
  }

  checkMonoidicalLaws(true, false, false)(boolAndMonoid)

  checkMonoidicalLaws(true, false, false)(boolOrMonoid)

  checkMonoidicalLaws(true, false, false)(boolEitherMonoid)

  checkMonoidicalLaws(true, false, false)(boolNorMonoid)
}
