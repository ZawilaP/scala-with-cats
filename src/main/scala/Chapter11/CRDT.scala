package Chapter11

object CRDT extends App {
  final case class GCounter(counters: Map[String, Int]) {
    def increment(machine: String, amount: Int): GCounter =
      GCounter(counters ++ (machine -> (amount + counters.getOrElse(machine, 0))))

    def merge(that: GCounter): GCounter = GCounter(that.counters ++ this.counters.map {
      case (k, v) =>
        k -> (v max that.counters.getOrElse(k, 0))
    })

    def total: Int = counters.values.sum
  }

  import cats.kernel.CommutativeMonoid

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
}
