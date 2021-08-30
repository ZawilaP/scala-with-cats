package Chapter2

import cats.Monoid
import cats.instances.int._
import cats.syntax.semigroup._

object SuperAdder extends App {
  def add[A](items: List[A])(implicit ev: Monoid[A]): A =
    items.foldLeft(ev.empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)

  implicit def orderMonoid: Monoid[Order] = new Monoid[Order] {
    def empty: Order = Order(0, 0)

    def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }
}
