package chapter_02

import cats.effect.{ExitCode, IO, IOApp}

object TruthAboutMonoids extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    IO {

      val r1 = associativeLaw(true, false, true)(MonoidInstances.booleanInstances2)
      val r2 = identityLaw(true)(MonoidInstances.booleanInstances2)

      println(r1)
      println(r2)

      val b1 = true
      val b2 = true

      println(b1 ^ b2)

    }.map(_ => ExitCode.Success)

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
  }

  object MonoidInstances {

    val booleanInstances1: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean = true

      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

    val booleanInstances2: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean = false

      override def combine(x: Boolean, y: Boolean): Boolean = x || y
    }

  }

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean =
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean =
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)

}
