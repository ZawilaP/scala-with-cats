package Chapter10

import cats.Semigroup
import cats.data.Validated
import cats.syntax.semigroup._
import cats.syntax.apply._
import cats.data.Validated._
import cats.implicits.catsSyntaxValidatedId // for Valid and Invalid


sealed trait Predicate[E, A] {
  import Predicate._
  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Or(this, that)

  def run(implicit s: Semigroup[E]): A => Either[E, A] = (a: A) => this(a).toEither

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(func) =>
      func(a)
    case And(left, right) =>
      (left(a), right(a)).mapN((_, _) => a)
    case Or(left, right) =>
      left(a) match {
        case Valid(_) => Valid(a)
        case Invalid(e1) =>
          right(a) match {
            case Valid(_) => Valid(a)
            case Invalid(e2) => Invalid(e1 |+| e2)
          }
      }
  }
  }

object Predicate {
  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
  final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]
  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] = Pure(a => if(fn(a)) a.valid else err.invalid)
  def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] =
    Pure(f)
}

