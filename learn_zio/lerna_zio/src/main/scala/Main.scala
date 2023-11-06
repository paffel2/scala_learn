import scala.io.StdIn.readLine
import scala.concurrent.Future
import scala.util.Try
import zio._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
//import zio.test.Assertion._

object MonadIOExample extends App {
  case class IO[A](run: () => A) {
    def map[B](f: A => B): IO[B] =
      IO(() => f(run()))

    def flatMap[B](f: A => IO[B]) = f(run())
  }

  val askForName: IO[Unit] = IO(() => println("Как тебя зовут?"))
  val readName: IO[String] = IO(() => readLine())
  def greet(name: String) = IO(() => println(s"Привет, $name!"))

  def greetByName: IO[Unit] = for {
    _ <- askForName
    name <- readName
    _ <- greet(name)
  } yield ()

  greetByName.run()

}

object Main extends App {
  val future: Future[Int] = Future(132)
  val z1 = zio.ZIO.fromFuture(ec => future)

  val zt = ZIO.fromTry(Success(132))

  val zo = ZIO.fromOption(Some(123))

  def askForName = Console.printLine("Как тебя зовут?")

  def readName = Console.readLine

  def greet(name: String): Task[Unit] = Console.printLine(s"Привет, $name!")

  def greater: Task[Unit] = for {
    _ <- askForName
    name <- readName
    _ <- greet(name)
  } yield ()

  greater.zipPar(askForName)

  // object FirstTest extends ZIOSpecD
  // val a = ZIO.attempt()
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(greater).getOrThrowFiberFailure()
  }

  /*Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe
      .run(ZIO.attempt(println("Hello World!")))
      .getOrThrowFiberFailure()
  }*/

  /*println(z1)
  z1 match {
    case Success(value) => println(value)
    case _              => println("error")
  }*/
}
