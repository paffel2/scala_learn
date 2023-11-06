import zio._
import zio.test._
import Main._

object SomeSpec extends ZIOSpecDefault {

  def spec = suite("test ask")(
    test("correct working") {
      for {
        _ <- TestConsole.feedLines("123")
        _ <- greater
        output <- TestConsole.output
      } yield assertTrue(
        output.size == 2
      )
    }
  )

}
