package chapter_04

import cats._
import cats.implicits._
import cats.data.State
import cats.effect.{ExitCode, IO, IOApp}

import scala.util.{Failure, Success, Try}

object PostOrderCalculator extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO {
//      val program1 = for {
//        _   <- evalOne("1")
//        _   <- evalOne("2")
//        ans <- evalOne("+")
//      } yield ans
//
//      val res1 = program1.runA(Nil).value
//      println(res1)

//      val program2 = evalAll(List("1", "2", "+", "3", "*"))
//      val res2     = program2.runA(Nil).value
//      println(res2)

      println(evalInput("1 2 + 3 4 + *"))
    }.map(_ => ExitCode.Success)

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    State(state =>
      sym match {
        case "+" =>
          val sum = state.last + state(state.length - 2)
          (state.slice(0, state.length - 2) :+ sum, sum)
        case "*" =>
          val prod = state.last * state(state.length - 2)
          (state.slice(0, state.length - 2) :+ prod, prod)
        case n =>
          Try(n.toInt) match {
            case Failure(exception) => throw exception
            case Success(value)     => (state :+ value, value)
          }
      }
    )

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(State.empty[List[Int], Int])((a, b) => a.flatMap(_ => evalOne(b)))

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

}
