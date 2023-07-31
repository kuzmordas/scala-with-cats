package chapter_02

import cats.effect.{ExitCode, IO, IOApp}
import cats._

object AddingAllTheThings extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO {

      val nums = List[Int](1, 2, 3, 4, 5)
      println(add(nums))

      val optNums = nums.map(n => Option(n))
      println(add(optNums))

      val orders = List[Order](Order(1, 1), Order(2, 1), Order(3, 1))
      println(add(orders))

    }.map(_ => ExitCode.Success)

//  def add(items: List[Int]): Int = items.foldLeft(Monoid[Int].empty)(Monoid[Int].combine)
//
//  def add(items: List[Option[Int]]): Int =
//    items.foldLeft(Monoid[Option[Int]].empty)(Monoid[Option[Int]].combine).getOrElse(0)

  def add[A: Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty)(Monoid[A].combine)

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(0, 0)

    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

}
