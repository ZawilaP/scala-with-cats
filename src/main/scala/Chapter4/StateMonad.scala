package Chapter4

import cats.data.State

object StateMonad extends App {
  type CalcState[A] = State[List[Int], A]

  def operand(num: Int): CalcState[Int] = State[List[Int], Int](stack => (num :: stack, num))

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ => throw new Error("Insuficcient number of entries in list")
    }

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case "%" => operator(_ % _)
      case "^" => operator(math.pow(_, _).floor.toInt)
      case num => operand(num.toInt)
    }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(State[List[Int], Int](_ => (List(), 0))) {
      (a, b) => a.flatMap(_ => evalOne(b))
    }

  def evalInput(input: String): Int = evalAll(input.toList.map(_.toString)).runA(Nil).value

  val program = evalAll(List("1", "2", "+", "3", "*"))

  println(program.runA(Nil).value == 9)

  val program2 = evalAll(List("1", "2", "^", "2", "%", "3", "+"))

  println(program2.runA(Nil).value == 4)

  println(evalInput("+"))
}
