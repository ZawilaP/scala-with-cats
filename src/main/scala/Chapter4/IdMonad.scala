package Chapter4
import cats.Id
//import Chapter4.Monad.Monad

import scala.language.higherKinds

object IdMonad extends App {
//  Failed first take
//  type Id[A] = A
//
//  implicit def IdMonad[A](implicit ev: Id[A]): Monad[Id[A]] = new Monad[Id] {
//    def pure[A](a: A): Id[A] = a
//
//    def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)
//  }
//
//  def map[A, B](value: A)(func: A => B)(implicit ev: Monad[Id[A]]): Id[B] = func(value)
  def pure[A](value: A): Id[A] = value

  def map[A, B](value: A)(func: A => B): Id[B] = func(value)

  def flatMap[A,B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)

  
}
