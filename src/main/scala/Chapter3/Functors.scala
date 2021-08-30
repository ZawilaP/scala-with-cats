package Chapter3

import cats.Functor
import cats.syntax.functor._

import scala.language.higherKinds

object Functors extends App {
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)
    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](value: Tree[A])(func: A => B): Tree[B] = value match {
      case Leaf(value) => Leaf(func(value))
      case Branch(left, right) => Branch(map(left)(func), map(right)(func))
    }
  }

  val testTree: Branch[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

  println(Tree.branch(Tree.leaf(1), Tree.branch(Tree.leaf(2), Tree.leaf(3))).map(_ % 2) )
}
