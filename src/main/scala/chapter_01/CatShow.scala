package chapter_01

import cats.effect.{ExitCode, IO, IOApp}
import cats._
import cats.implicits._

object CatShow extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO {

      implicit val catShow: Show[Cat] =
        Show.show(cat => s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat.")

      val cat = Cat("Lucky", 3, "black")

      println(cat.show)

    }.map(_ => ExitCode.Success)

  final case class Cat(name: String, age: Int, color: String)
}
