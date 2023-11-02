import cats._
import cats.implicits._
//import cats.Monoid
//import cats.syntax._

object Monoids extends App {

  case class SomeNum(value: Int)

  implicit val someNumMonoid: Monoid[SomeNum] = {
    new Monoid[SomeNum] {

      def empty: SomeNum = SomeNum(1)

      def combine(x: SomeNum, y: SomeNum): SomeNum = SomeNum(x.value + y.value)

    }
  }

  println(
    Monoid.combine(SomeNum(12), Monoid.empty[SomeNum])
  ) // законы не проверяются

  case class OrMonoid(value: Boolean)
  case class AndMonoid(value: Boolean)

  object MonoidInstances {
    implicit val orMonoidinstance: Monoid[OrMonoid] = {
      new Monoid[OrMonoid] {

        def empty: OrMonoid = OrMonoid(false)

        def combine(x: OrMonoid, y: OrMonoid): OrMonoid = OrMonoid(
          x.value || y.value
        )

      }
    }

    implicit val andMonoidinstance: Monoid[AndMonoid] = {
      new Monoid[AndMonoid] {

        def empty: AndMonoid = AndMonoid(true)

        def combine(x: AndMonoid, y: AndMonoid): AndMonoid = AndMonoid(
          x.value && y.value
        )

      }
    }

  }
  val res: Option[Int] = (Some(123): Option[Int]) |+| Some(1)
  println(res)
}

object SuperAdder extends App {

  def add(items: List[Int]): Int = items.foldLeft(Monoid[Int].empty)(_ |+| _)

  def addOption(items: List[Option[Int]]): Int =
    items.foldLeft(Monoid[Option[Int]].empty)(_ |+| _) match {
      case None        => 0
      case Some(value) => value
    }

  // println(addOption(List[Option[Int]](None, Some(1), Some(3))))

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoidInstance: Monoid[Order] = new Monoid[Order] {

    def empty: Order = Order(0, 0)
    def combine(x: Order, y: Order) =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  println(add(List(Order(1, 1), Order(2, 2))))
}
