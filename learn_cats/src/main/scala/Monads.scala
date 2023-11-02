import cats.implicits._
import cats._
import scala.util.Try
import cats.data.Writer
import cats.data.Reader

object Monads extends App {

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(func.map(pure))

  }
  def validateAdult[F[_]](age: Int)(implicit
      me: MonadError[F, Throwable]
  ): F[Int] = if (age >= 18) age.pure[F]
  else
    new IllegalArgumentException("Age must be greater than or equal to 18")
      .raiseError[F, Int]

  println(validateAdult[Try](18))
  println(validateAdult[Try](8))

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(
      fn: (A, Eval[B]) => Eval[B]
  ): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value

  type Logged[A] = Writer[Vector[String], A]

  def factorial(n: Int): Logged[Int] = {
    val w: Logged[Int] = new Writer(Vector("fact 0 1"), 1)
    def loop(m: Int, w: Logged[Int]): Logged[Int] = if (m > n)
      w
    else {
      val c = for {
        a <- w
        _ <- Vector(s"fact $m ${a * m}").tell

      } yield (a * m)
      loop(m + 1, c)
    }
    loop(1, w)
  }
  // factorial(5).run._1.map(s => println(s))

  final case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def ask: DbReader[Db] = Reader(db => db)

  def findUsername(userId: Int): DbReader[Option[String]] = {
    Reader(db => db.usernames.get(userId))
  }

  def checkPassword(username: String, password: String): DbReader[Boolean] = {
    Reader(db => db.passwords.get(username).contains(password))
  }

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    userName <- findUsername(userId)
    result <- userName match {
      case None        => false.pure[DbReader]
      case Some(value) => checkPassword(value, password)
    }
  } yield result

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )
  val db = Db(users, passwords)
  println(checkLogin(1, "zerocool").run(db))
// res7: cats.package.Id[Boolean] = true
  println(checkLogin(4, "davinci").run(db))
// res8: cats.package.Id[Boolean] = false

}
