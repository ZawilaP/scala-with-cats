package Chapter11

import cats.instances.list._   // for Monoid
import cats.instances.map._    // for Monoid
import cats.syntax.semigroup._ // for |+|
import cats.syntax.foldable._  // for combineAll
import cats.kernel.CommutativeMonoid

object CRDT extends App {
  trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
    def combine(a1: A, a2: A): A

    def empty: A
  }

  object BoundedSemiLattice {
    implicit val intBSL: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
      def combine(a1: Int, a2: Int): Int = a1 max a2

      def empty: Int = 0
    }

    implicit def setBSL[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
      def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2

      def empty: Set[A] = Set.empty[A]
    }
  }

  final case class GCounter[A](counters: Map[String, A]) {
    def increment(machine: String, amount: A)(implicit m: CommutativeMonoid[A]): GCounter[A] = {
      val value = amount |+| counters.getOrElse(machine, m.empty)
      GCounter(counters + (machine -> value))
    }

    def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
      GCounter(this.counters |+| that.counters)

    def total(implicit m: CommutativeMonoid[A]): A = this.counters.values.toList.combineAll
  }
}
