import cats.effect._
import cats.implicits._

object Main extends App {
  case class LazyIO[A](runEffect: () => A) {
    def map[B](fn: A => B): LazyIO[B] = LazyIO.io(fn(runEffect()))

    def flatMap[B](fn: A => LazyIO[B]): LazyIO[B] =
      LazyIO.io(fn(runEffect()).runEffect())
  }

  object LazyIO {
    def io[A](effect: => A): LazyIO[A] = new LazyIO[A](() => effect)
  }

  val io = LazyIO.io(println("Effect started"))

  val twoRuns = for {
    _ <- io
    _ <- io

  } yield ()

  // twoRuns.runEffect()
}

object Printer extends IOApp {
  // def printSome(string: String): IO[Unit] = IO.delay(println(string))
  def printSome(string: String): IO[Unit] = IO(println(string))
  val launch = for {
    _ <- printSome("First some")
    _ <- printSome("Second some")
  } yield ()

  override def run(args: List[String] = List()): IO[ExitCode] =
    launch.as(ExitCode.Success)

  // run()
}

object TestTraversable extends IOApp {

  def printSome(string: String): IO[Unit] = IO(println(string))

  val tasks: List[Int] = (1 to 1000).toList

  def taskExecutor(i: Int): String = s"Executing task $i"

  def runTasks: IO[List[Unit]] = tasks.traverse(i => printSome(taskExecutor(i)))

  val deferIO: IO[Unit] = IO.defer(IO(println("IO in defer")))

  val c = IO("abc").flatTap(s => IO(println(s"123 $s")))

  def go = for {
    a <- c
    _ <- printSome(a)
  } yield ()

  override def run(args: List[String] = List()): IO[ExitCode] =
    go.as(ExitCode.Success)

}
