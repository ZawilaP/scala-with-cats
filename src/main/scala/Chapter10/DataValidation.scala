package Chapter10

import cats.Semigroup
import cats.implicits.catsSyntaxEitherId
import cats.syntax.semigroup._
object DataValidation extends App {
  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] = And(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] =
      this match {
        case Pure(func) => func(a)
        case And(left, right) =>
          (left(a), right(a)) match {
            case (Left(l1), Left(l2)) => (l1 |+| l2).asLeft
            case (Left(l), Right(_)) => l.asLeft
            case (Right(_), Left(l)) => l.asLeft
            case (Right(_), Right(_)) => a.asRight
          }
      }
  }

  final case class And[E, A](left: Check[E, A],  right: Check[E, A]) extends Check[E, A]
  final case class Pure[E, A](func: A => Either[E, A]) extends Check[E, A]

  val a: Check[List[String], Int] =
    Pure { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }

  val b: Check[List[String], Int] =
    Pure { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }

  val check: Check[List[String], Int] =
    a and b
}
