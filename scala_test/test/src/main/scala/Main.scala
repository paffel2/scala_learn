import scala.annotation.tailrec
import scala.io.StdIn._

object Main extends App {
  def powerOfTwo(base: Int): BigInt = {
    @tailrec
    def loop(base: Int, accum: BigInt): BigInt = {
      if (base > 1)
        loop(base - 1, accum * 2)
      else
        accum
    }
    if (base == 0)
      1
    else if (base == 1)
      2
    else
      loop(base, 2)
  }

  // println(powerOfTwo(2))

  def cycle(base: Int, count: Int, add: Int): Unit = {

    val result = base + count * add
    val n = result.toString().size
    val stringResult = (result.toString + " ") * n + "is the result"
    println(stringResult)

  }

  def reverseSentence() = {
    val words = readLine.split(raw"\s+")
    println(words.reverse.mkString(" "))

  }

  class Instructor(val id: Int, val name: String, val surname: String) {
    def fullName = s"${name.capitalize} ${surname.capitalize}"
  }

  class Course(
      val courseID: Int,
      val title: String,
      val releaseYear: String,
      val instructor: Instructor
  ) {
    def getID = List(courseID.toString, instructor.id.toString).mkString

    def isTaughtBy(instructor: Instructor): Boolean =
      instructor == this.instructor

    def copyCourse(newReleaseYear: String) =
      new Course(courseID, title, newReleaseYear, instructor)
  }

  val instruc = new Instructor(1, "ivan", "ivanov")
  val instruc2 = new Instructor(2, "Ivan", "Ivanov")
  val kurs = new Course(1, "football", "2023", instruc)
  val kurs2 = kurs.copyCourse("2024")
  println(kurs.getID)
  println(kurs.isTaughtBy(instruc))
  println(instruc.fullName)
}
