import scala.math.BigDecimal._
object Main extends App {

  case class Course(id: Int, title: String)

  val courses = List(
    Course(0, "Scala"),
    Course(1, "Functions"),
    Course(1, "Advanced Scala"),
    Course(4, "Spark"),
    Course(3, "Cats"),
    Course(3, "Akka")
  )

  /*implicit val courseOrdering: Ordering[Course] =
    Ordering.fromLessThan((a, b) => {
      if (a.id < b.id) true
      else if (a.id == b.id) { a.title < b.title }
      else false
    })*/
  implicit val CourseOrdering: Ordering[Course] =
    Ordering.by(c => (c.id, c.title))
  // println(courses.sorted)

  case class Person(age: Int) {
    def increaseAge: Unit = println(age + 1)
  }
  object Person {
    implicit def str2Person(param: String): Person = Person(param.toInt)
  }

  import Person._

  val age = "33"
  // age.increaseAge

  case class Price(price: Int)
  object Price {

    implicit class PriceOps(p: Price) {
      def -(n: Int): Price = Price(p.price - n)
    }

  }

  println(Price(500) - 50)

}
