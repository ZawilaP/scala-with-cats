package Chapter11
import Chapter11.KeyValueStore.KvsOps
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

  implicit def gcounterInstance[F[_, _], K, V]
  (implicit kvs: KeyValueStore[F], km: CommutativeMonoid[F[K, V]]): GCounter[F, K, V] =
    new GCounter[F, K, V] {
      def increment(f: F[K, V])(key: K, value: V)
                   (implicit m: CommutativeMonoid[V]): F[K, V] = {
        val total = f.getOrElse(key, m.empty) |+| value
        f.put(key, total)
      }
      def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = f1 |+| f2
      def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V = f.values.combineAll
    }
}

trait KeyValueStore[F[_,_]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
  def get[K, V](f: F[K, V])(k: K): Option[V]
  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V = get(f)(k).getOrElse(default)
  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {
  def apply[F[_,_]](implicit kvs: KeyValueStore[F]): KeyValueStore[F] = kvs

  implicit def mapKVS: KeyValueStore[Map] = new KeyValueStore[Map] {
    def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)
    def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)
    def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }
  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] = kvs.put(f)(key, value)
    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] = kvs.get(f)(key)
    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V = kvs.getOrElse(f)(key, default)
    def values(implicit kvs: KeyValueStore[F]): List[V] = kvs.values(f)
  }
}
