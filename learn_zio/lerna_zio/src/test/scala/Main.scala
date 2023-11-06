import zio._
import zio.test._
import zio.test.Assertion._
import scala.io.StdIn.readLine
import java.io.IOException

object SomeSpec extends ZIOSpecDefault {

  import Main.{greater => testable}

  def spec = suite("test ask")(
    test("correct working") {
      for {
        _ <- TestConsole.feedLines("123")
        _ <- testable
        output <- TestConsole.output
      } yield assertTrue(
        output.size == 2
      )
    }
  )

}
