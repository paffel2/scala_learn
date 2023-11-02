import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

object Main extends App {

  val future = Future {
    // прежде чем вернуть число 12 - выполняются вычисления, занимающие длительное время
    12
  }

  future.onComplete { case Success(result) =>
    println(result)
  }
}
