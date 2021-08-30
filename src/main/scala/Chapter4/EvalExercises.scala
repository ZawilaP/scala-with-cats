package Chapter4

import cats.Eval


object EvalExercises extends App{
  def unsafeFoldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, unsafeFoldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  def safeFoldRight[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, safeFoldRight(tail, acc)(fn)))
      case Nil =>
        acc
    }

  val hugeList: List[Int] = (1 to 500000).toList

  println("Safe case:")
  println(safeFoldRight(as = hugeList, acc = Eval.Zero)((a: Int, b: Eval[Int]) => b.map(_ + a)).value)

  println("Unsafe case:")
  println(unsafeFoldRight(as = hugeList, acc = 0)(_ + _))
}
