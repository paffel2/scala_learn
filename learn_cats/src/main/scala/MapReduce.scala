import cats.syntax.all._
import cats.Monoid
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.future._
import cats.instances.list._
import scala.concurrent.duration._
import scala.concurrent.Await
import cats.Foldable

object MapReduce extends App {

  def myFoldMap[A, B: Monoid](init: Vector[A])(f: A => B): B =
    init.foldLeft(Monoid[B].empty)(_ |+| f(_))

  val q = List(Future(1), Future(2), Future(3)).sequence

  // println(Runtime.getRuntime.availableProcessors) - 8

  // (1 to 10).toList.grouped(3).toList

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(
      func: A => B
  ): Future[B] = {

    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (values.size / numCores)

    val groups = values.grouped(groupSize)
    val futures =
      groups.map(group =>
        Future(group.foldLeft(Monoid[B].empty)(_ |+| func(_)))
      )
    // println(futures.size)

    Future
      .sequence(futures)
      .map(value => value.foldLeft(Monoid[B].empty)(_ |+| _))
  }

  def catsParallelFoldMap[A, B: Monoid](
      values: Vector[A]
  )(func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (values.size / numCores)
    val groups = values.grouped(groupSize)
    groups.toList.foldMap(group => Future(group.foldMap(v => func(v))))

    /*val r = groups.toList
      .traverse(group => Future(group.foldMap(v => func(v))))
      .map(_.combineAll)*/

  }
  lazy val result1: Future[Int] =
    parallelFoldMap((1 to 1000).toVector)((_ * 1000))

  lazy val result2: Future[Int] =
    catsParallelFoldMap((1 to 1000).toVector)((_ * 1000))

  println(Await.result(result1, 1.second))
  println(Await.result(result2, 1.second))
  // println((1 to 80).sum)

}
