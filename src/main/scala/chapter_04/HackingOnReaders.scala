package chapter_04

import cats._
import cats.implicits._
import cats.data.Reader
import cats.effect.{ExitCode, IO, IOApp}

object HackingOnReaders extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO {

      val users = Map(
        1 -> "dade",
        2 -> "kate",
        3 -> "margo"
      )

      val passwords = Map(
        "dade"  -> "zerocool",
        "kate"  -> "acidburn",
        "margo" -> "secret"
      )

      val db = Db(users, passwords)

      val res1 = checkLogin(1, "zerocool").run(db)
      println(res1)

      val res2 = checkLogin(4, "davinci").run(db)
      println(res2)

    }.map(_ => ExitCode.Success)

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).map(_ == password).getOrElse(false))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      optUser <- findUsername(userId)
      res <- optUser match {
        case Some(user) => checkPassword(user, password)
        case None       => false.pure[DbReader]
      }
    } yield res

}
