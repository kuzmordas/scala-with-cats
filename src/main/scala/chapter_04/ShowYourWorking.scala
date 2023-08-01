package chapter_04

import cats.data.Writer
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object ShowYourWorking extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO {

      val r = Await.result(Future.sequence(
                             Vector(
                               Future(factorial(3)),
                               Future(factorial(3))
                             )
                           ),
                           5.seconds)
      println(r)

//      val x = factorial(5).run
//      println(x)

    }.map(_ => ExitCode.Success)

  def slowly[A](body: => A): A =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Writer[Vector[String], Int] = slowly {
    if (n == 0)
      Writer(Vector(f"fact $n 1"), 1)
    else
      factorial(n - 1).flatMap(x => Writer(Vector(f"fact $n ${x * n}"), x * n))
  }

}
