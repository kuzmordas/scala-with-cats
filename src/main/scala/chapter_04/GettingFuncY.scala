package chapter_04

import cats.effect.{ExitCode, IO, IOApp}

object GettingFuncY extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = IO {}.map(_ => ExitCode.Success)

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(a => pure(func(a)))
  }
}
