package Chapter9

import cats.Monoid
import cats.implicits.{catsSyntaxSemigroup, toFoldableOps, toTraverseOps}
import cats.instances.int._
import cats.instances.string._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

object MapReduce extends App {
  def foldMap[A,B: Monoid](values: Vector[A])(f: A => B): B =
    values.foldLeft(Monoid[B].empty)(_ |+| f(_))

  println(foldMap(Vector(1, 2, 3))(identity))
  println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
  println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))

  val future1 = Future {
    (1 to 100).toList.sum
  }
  // future1: scala.concurrent.Future[Int] = Future(<not completed>)
  val future2 = Future {
    (100 to 200).toList.sum
  }
  // future2: scala.concurrent.Future[Int] = Future(<not completed>)

  val future3 = future1.map(_.toString)
  // future3: scala.concurrent.Future[String] = Future(<not completed>)
  val future4 = for {
    a <- future1
    b <- future2
  } yield a + b
  // future4: scala.concurrent.Future[Int] = Future(<not completed>)

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val futures = values
      .grouped((1.0 * values.size / Runtime.getRuntime.availableProcessors).ceil.toInt)
      .map(x => Future(foldMap(x)(func)))

    Future.sequence(futures).map(el => el.foldLeft(Monoid[B].empty)(_ |+| _))
  }

  val result: Future[Int] =
    parallelFoldMap((1 to 1000000).toVector)(identity)

  println(Await.result(result, 1.second))
  // res21: Int = 1784293664

  def parallelFoldMap2[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    values
      .grouped((1.0 * values.size / Runtime.getRuntime.availableProcessors).ceil.toInt)
      .toVector
      .traverse(el => Future(el.foldMap(func)))
      .map(_.combineAll)
  }

  val future: Future[Int] =
    parallelFoldMap((1 to 1000).toVector)(_ * 1000)

  println(Await.result(future, 1.second))
  // res3: Int = 500500000
}
