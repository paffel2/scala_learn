import zio._
import zio.stream._

object ZioStreams extends App {

  val simpleStream = ZStream.range(0, 1000)
  val sumZSink = ZSink.sum[Int]
  val takeZSink = ZSink.take[Int](3)

  val processedSumStream = simpleStream.run(sumZSink)

  val processedTakeStream = simpleStream.run(takeZSink)

  val processSum = for {
    values <- processedSumStream
    _ <- Console.printLine(values)
  } yield ()

  val processTake = for {
    values <- processedTakeStream
    _ <- values.mapZIOPar(s => Console.printLine(s))

  } yield ()

  val failingStream: ZStream[Any, Throwable, Int] = ZStream.range(0, 5) ++
    ZStream.fail(new RuntimeException("Failing!")) ++
    ZStream.range(6, 10)
  val recoveryStream: ZStream[Any, Throwable, Int] = ZStream.range(10, 15)

  def run: ZIO[Any, Throwable, Int] =
    failingStream.orElse(recoveryStream).runSum

  val failingProcess = for {
    value <- run
    _ <- Console.printLine(value)
  } yield ()

  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(failingProcess).getOrThrowFiberFailure()
  }

  println(Range.inclusive(10, 15).sum)

}

object cooking extends App {

  def delay(duration: Duration): ZIO[Any, Nothing, Unit] = ZIO.sleep(duration)

  def boilWater(): ZIO[Any, Nothing, Unit] =
    for {
      _ <- ZIO.succeed(println("Water put on stove..."))
      _ <- delay(100.milli)
      _ <- ZIO.succeed(println("Water boiled!"))
    } yield ()

  def boilPasta(): ZIO[Any, Nothing, Unit] =
    for {
      _ <- ZIO.succeed(println("Put pasta in boiling water..."))
      _ <- delay(1.second)
      _ <- ZIO.succeed(println("Pasta ready!"))
    } yield ()

  def prepareIngredient(ingredient: String): ZIO[Any, Nothing, Unit] =
    for {
      _ <- ZIO.succeed(println(s"Preparing $ingredient..."))
      _ <- delay(300.millis)
      _ <- ZIO.succeed(println(s"$ingredient ready"))
    } yield ()

  def makeSauce(): ZIO[Any, Nothing, Unit] =
    for {
      _ <- ZIO.succeed(println(s"Preparing sauce..."))
      _ <- delay(1.second)
      _ <- ZIO.succeed(println(s"Sauce is ready"))
    } yield ()

  def badPastaCook: ZIO[Any, Nothing, Unit] = for {
    waterFiber <- boilWater().fork
    pastaFiber <- boilPasta().fork
    tomatoFiber <- prepareIngredient("tomato").fork
    onionFiber <- prepareIngredient("onion").fork
    suaceFiber <- makeSauce().fork

  } yield ()

  def pastaCook: ZIO[Any, Nothing, Unit] = for {
    waterFiber <- boilWater().fork
    pastaFiber <- waterFiber.await.zip(boilPasta()).fork
    tomatoFiber <- prepareIngredient("tomato").fork
    onionFiber <- tomatoFiber.await.zip(prepareIngredient("onion")).fork
    sauceFiber <- onionFiber.await.zip(makeSauce()).fork
    _ <- pastaFiber.zip(sauceFiber).join
  } yield ()

  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(pastaCook).getOrThrowFiberFailure()
  }
}
