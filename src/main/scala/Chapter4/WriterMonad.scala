package Chapter4
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import cats.data.Writer
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.vector._

object WriterMonad extends App{
  def slowly[A](body: => A): A = try body finally Thread.sleep(100)
  def factorial(n: Int): Int = {
    val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  Await.result(Future.sequence(Vector(
    Future(factorial(3)),
    Future(factorial(3))
  )), 5.seconds)

  type FactorialLog[A] = Writer[Vector[String], A]

  def logSafeFactorial(n: Int): FactorialLog[Int] = {
    for {
      value <- if(n==0) {1.pure[FactorialLog]} else {slowly(logSafeFactorial(n - 1).map(_ * n))}
      _ <- Vector(s"Factorial for $n is equal to $value").tell
    } yield value
  }

  val (logs, value) = logSafeFactorial(10).run

  println(logs)
  println(value)

  val results: Seq[(Vector[String], Int)] = Await.result(Future.sequence(Vector(
    Future(logSafeFactorial(3).run),
    Future(logSafeFactorial(3).run)
  )), 5.seconds)

  println(results.head._1)
  println(results.head._2)
  println(results.last._1)
  println(results.last._2)
}
