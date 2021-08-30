package Chapter5

import cats.data.EitherT
import cats.implicits.catsSyntaxApplicativeId
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object MonadTransformers extends App {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(power) => EitherT.right(Future(power))
      case None => EitherT.left(Future(s"Autobot $autobot is unreachable at this moment"))
    }

  def canSpecialMove(autobot1: String, autobot2: String): Response[Boolean] = {
    (Await.result(getPowerLevel(autobot1).value, 1.second), Await.result(getPowerLevel(autobot2).value, 1.second)) match {
      case (Right(power1), Right(power2)) if power1 + power2 > 15 => true.pure[Response]
      case (Right(power1), Right(power2)) if power1 + power2 <= 15 => false.pure[Response]
      case _ => throw new Error(s"On of the autobots: $autobot1, $autobot2, was unavailable.")
    }
  }

  def tacticalReport(autobot1: String, autobot2: String): String = {
    Await.result(canSpecialMove(autobot1, autobot2).value, 1.second) match {
      case Right(true) => s"Autobots: $autobot1 and $autobot2 can perform a special move!"
      case _ => s"Autobots: $autobot1 and $autobot2 cannot perform a special move!"
    }
  }

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))
}
