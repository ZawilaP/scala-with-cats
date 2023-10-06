package Chapter7

import cats.Monoid

object Foldable extends App {
  val leftList = List(1, 2, 3).foldLeft(List.empty[Int])((acc, el) => el :: acc)
  val rightList = List(1, 2, 3).foldRight(List.empty[Int])((el, acc) => el :: acc)

  def map[A, B](list: List[A])(func: A => B): List[B] = {
    list.foldRight(List.empty[B])((el, acc) => func(el) :: acc)
  }

  def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] = {
    list.foldRight(List.empty[B])((el, acc) => func(el) ::: acc)
  }

  def filter[A](list: List[A])(func: A => Boolean): List[A] = {
    list.foldRight(List.empty[A])((el, acc) => if(func(el)) el :: acc else acc)
  }

  def sum[A](list: List[A])(implicit mon: Monoid[A]): A = list.foldRight(mon.empty)(mon.combine)

  println(map(List(1,2,3))(_ * 2))

  println(flatMap(List(1,2,3))(_ :: List(1,2,3)))

  println(filter(List(1,2,3))(_ % 2 == 0))

  println(sum(List(1,2,3)))
}
