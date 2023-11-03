import cats.implicits._
import cats._
import scala.util.Try
import cats.syntax._
import cats.data.OptionT
import cats.data.ReaderT
import cats.data.EitherT
import cats.data.Reader
import scala.concurrent.Future
import scala.concurrent.duration._
import cats.instances.future._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await

object Transformers extends App {

  type ListOption[A] = OptionT[List, A]

  val result: ListOption[Int] = 32.pure[ListOption]

  val resultNew = for {
    x <- result
  } yield x + 1

  type ErrorOr[A] = Either[String, A]

  type ReaderInt[A] = Reader[Int, A]

  type ErrorOrReader[A] = EitherT[ReaderInt, String, A]

  val result3: ErrorOrReader[Char] = 'a'.pure[ErrorOrReader]

  // println(result3.value.run(123))

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(value) => EitherT.right(Future(value))
      case None        => EitherT.left(Future(s"$autobot unreachable"))
    }
  }
  def canSpecialMove1(ally1: String, ally2: String): Response[Boolean] = {
    val powerLevel1 = powerLevels.get(ally1)
    val powerLevel2 = powerLevels.get(ally2)
    (powerLevel1, powerLevel2) match {
      case (Some(value1), Some(value2)) =>
        if (value2 + value1 > 15)
          true.pure[Response]
        else false.pure[Response]
      case (None, _) => EitherT.left(Future((s"$ally1 unreachable")))
      case (_, None) => EitherT.left(Future((s"$ally2 unreachable")))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
    power1 <- getPowerLevel(ally1)
    power2 <- getPowerLevel(ally2)

  } yield (power1 + power2) > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val report = canSpecialMove1(ally1, ally2).value
    Await.result(report, 2.second) match {
      case Left(error)    => s"Error: $error"
      case Right(message) => s"$ally1 and $ally2 reports: $message"

    }

  }

}
