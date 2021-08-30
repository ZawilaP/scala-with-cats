package Chapter4

import Chapter4.Monad.Monad

object TreeMonad {
  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  val treeMonad: Monad[Tree] = new Monad[Tree] {
    def pure[A](x: A): Tree[A] = leaf(x)

    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value) => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = flatMap(f(a)){
      case Left(value) => tailRecM(value)(f)
      case Right(value) => Leaf(value)
    }
  }
}
