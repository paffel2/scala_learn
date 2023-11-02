import cats._ //импортим все из cats
import cats.implicits._ //импортим неявные значения

import java.util.Date
object ShowCats extends App {

  val showInt: Show[Int] = Show.apply[Int]

  // println(123.show)

  /*implicit val dateShow: Show[Date] = //сложный способ
    new Show[Date] {
      def show(date: Date): String = s"${date.getTime()}ms since the epoch."
    }*/

  implicit val dateShow: Show[Date] =
    Show.show(date => s"${date.getTime()}ms since the epoch.") // способ проще
  // println(new Date().show)

  case class Cat(name: String, age: Int, color: String)

  implicit val catsShow: Show[Cat] =
    Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")

  // println(Cat("Tessi", 1, "Red").show)

  implicit val catsEq: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      cat1.name === cat2.name && cat1.age === cat2.age && cat1.color === cat2.color
    }

  println(Cat("Tessi", 1, "Red") =!= Cat("Tessi", 2, "Red"))
}
