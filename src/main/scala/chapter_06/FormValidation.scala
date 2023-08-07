package chapter_06

import cats.data.Validated
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}

import scala.util.{Failure, Success, Try}

object FormValidation extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO {
      val form = Map("name" -> "", "age" -> "-5")
      println(readUser(form))
    }.map(_ => ExitCode.Success)

  type ErrOrValue[A] = Either[List[String], A]

  case class User(name: String, age: Int)

  def readName(value: Map[String, String]): ErrOrValue[String] =
    value.get("name") match {
      case Some(name) => if (name.isEmpty) List("'name' should be not empty").asLeft else name.asRight
      case None       => List("'name' should be specified").asLeft
    }

  def readAge(value: Map[String, String]): ErrOrValue[Int] =
    value.get("age") match {
      case Some(age) =>
        if (age.isEmpty)
          List("'age' should be not empty").asLeft
        else
          Try(age.toInt) match {
            case Failure(_) => List("'age' should be an integer").asLeft
            case Success(value) =>
              if (value < 0)
                List("'age' should be positive").asLeft
              else
                value.asRight
          }
      case None => List("'age' should be specified").asLeft
    }

  def readUser(form: Map[String, String]): Validated[List[String], User] =
    (readName(form).toValidated, readAge(form).toValidated).mapN(User.apply)

}
