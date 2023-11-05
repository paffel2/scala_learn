import cats.Semigroup
import cats.syntax.all._
import cats.data.Validated

object DataValidation extends App {

  case class Check[E: Semigroup, A](func: A => Either[E, A]) {

    def apply(value: A): Either[E, A] = func(value)

    def and(that: Check[E, A]): Check[E, A] = Check { a =>
      (this(a), that(a)) match {
        case (Left(e1), Left(e2)) => Left(e1 |+| e2)
        case (Left(e), _)         => Left(e)
        case (_, Left(e))         => Left(e)
        case (Right(_), Right(_)) => Right(a)
      }
    }
  }

  trait CheckT[E, A, B] {
    def apply(a: A): Validated[E, B] = ???

    def map[C](func: B => C): CheckT[E, A, C] = ???
  }
  def toEmail(value: String): Either[Int, String] = if (value == "e@mail.com")
    Right("good")
  else
    Left(0)

  val email = Check[Int, String](toEmail)

  println(email(""))
}
