package chapter_01

import cats.effect.{ExitCode, IO, IOApp}

import cats._
import cats.implicits._

object Equality extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    IO {

      implicit val catEq: Eq[Cat] = Eq.instance((cat1, cat2) => {
        val isNameEq  = cat1.name === cat2.name
        val isAgeEq   = cat1.age === cat2.age
        val isColorEq = cat1.color === cat2.color
        isNameEq && isAgeEq && isColorEq
      })

      val cat1 = Cat("Garfield", 38, "orange and black")
      val cat2 = Cat("Garfield", 38, "orange and black")

      val optCat1 = Option(cat1)
      val optCat2 = Option.empty[Cat]
      val optCat3 = Option(cat2)

      println(cat1 === cat2)
      println(optCat1 === optCat2)
      println(optCat1 === optCat3)

    }.map(_ => ExitCode.Success)

  final case class Cat(name: String, age: Int, color: String)

}
