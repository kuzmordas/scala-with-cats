package chapter_01

import cats.effect.{ExitCode, IO, IOApp}

object PrintableLibrary extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    IO {
      import PrintableInstances._
      import PrintableSyntax._

      val cat = Cat("Lucky", 3, "black")
      cat.print

    }.map(_ => ExitCode.Success)

  trait Printable[A] {
    def format(value: A): String
  }

  final case class Cat(name: String, age: Int, color: String)

  object PrintableInstances {
    implicit val stringPrintable: Printable[String] = new Printable[String] {
      override def format(value: String): String = value
    }

    implicit val intPrintable: Printable[Int] = new Printable[Int] {
      override def format(value: Int): String = value.toString
    }

    implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
      override def format(value: Cat): String = s"${value.name} is a ${value.age} year-old ${value.color} cat."
    }
  }

  object Printable {
    def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

    def print[A](value: A)(implicit p: Printable[A]): Unit = println(format(value))
  }

  object PrintableSyntax {

    implicit class PrintableOps[A](value: A) {
      def format(implicit p: Printable[A]): String = p.format(value)

      def print(implicit p: Printable[A]): Unit = println(p.format(value))
    }

  }

}
