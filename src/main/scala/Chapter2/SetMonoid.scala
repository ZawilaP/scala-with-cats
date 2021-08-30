package Chapter2

import Chapter2.MonoidLaws.{Monoid, Semigroup}

object SetMonoid {
  def setUnionMonoid[A](): Monoid[Set[A]] = new Monoid[Set[A]] {
    def empty: Set[A] = Set.empty[A]

    def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  def setIntersectionSemigroup[A](): Semigroup[Set[A]] = new Semigroup[Set[A]] {
    def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
  }


}
