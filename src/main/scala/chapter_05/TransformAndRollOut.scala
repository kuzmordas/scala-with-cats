package chapter_05

import cats._
import cats.implicits._
import cats.data.EitherT
import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object TransformAndRollOut extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO {

      println(tacticalReport("Jazz", "Hot Rod"))

    }.map(_ => ExitCode.Success)

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(value) => EitherT.right(Future(value))
      case None        => EitherT.left(Future("error"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      l1 <- getPowerLevel(ally1)
      l2 <- getPowerLevel(ally2)
    } yield l1 + l2 > 15

  def tacticalReport(ally1: String, ally2: String): String =
    Await
      .result(canSpecialMove(ally1, ally2).map {
        case false => "need a recharge"
        case true  => "ready to roll out"
      }.value, 3.seconds)
      .merge

}
