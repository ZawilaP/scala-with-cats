package Chapter11

import Chapter11.CRDT.BoundedSemiLattice
import cats.implicits.{catsSyntaxSemigroup, toFoldableOps}
import cats.kernel.CommutativeMonoid

import scala.language.higherKinds

trait GCounter[F[_,_],K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}
object GCounter {
  def apply[F[_,_], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] = counter

  implicit def mapGCounter[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
    def increment(f: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
      val value = v |+| f.getOrElse(k, m.empty)
      f + (k -> value)
    }

    def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] = f1 |+| f2

    def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V = f.values.toList.combineAll
  }
}
