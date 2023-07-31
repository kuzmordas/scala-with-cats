package chapter_03

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}

object BranchingOutWithFunctors extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO {
      val tree: Tree[Int] = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))

      println(tree.map(x => x + 1))

    }.map(_ => ExitCode.Success)

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value)         => Leaf(f(value))
      }
  }

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A)                        extends Tree[A]

}
