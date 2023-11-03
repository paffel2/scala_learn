import cats.implicits._
import cats._
import cats.syntax.flatMap

object Applicative extends App {

  def mapList[A, B](list: List[A], f: A => B): List[B] =
    list.foldRight(List.empty[B])((value, acc) => f(value) :: acc)

  // val a = List(1, 2, 3).flatMap()

  def foldFlatMap[A, B](list: List[A], f: A => List[B]): List[B] =
    list.foldRight(List.empty[B])((value, acc) => f(value) ::: acc)

  // println(mapList[Int, Int](List(1, 2, 3), (s => s + 1)))

  def foldFilter[A](list: List[A])(p: A => Boolean): List[A] = {
    list.foldRight(List.empty[A])((value, acc) =>
      if (p(value)) value :: acc
      else acc
    )
  }
  // println(foldFlatMap[Int, Int](List(1, 2, 3), a => List(a, a * 10, a * 100)))

  // println(foldFilter(List(1, 2, 3))(_ % 2 == 1))

  def foldSum[A: Numeric](list: List[A])(implicit numeric: Numeric[A]): A =
    list.foldRight(numeric.zero)(numeric.plus)

  // println(foldSum(List(1, 2, 3)))
}
