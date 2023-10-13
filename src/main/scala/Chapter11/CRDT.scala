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
}
