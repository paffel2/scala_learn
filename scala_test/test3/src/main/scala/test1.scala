object test1 extends App {

  def guard(data: Any, maxLength: Int): String = {
    data match {
      case list: List[Any] => {
        if (list.size <= maxLength)
          "Задан список List допустимой длины"
        else
          "Длина списка больше максимально допустимого значения"
      }
      case str: String => {
        if (str.size <= maxLength)
          "Длина строки не превышает максимально допустимого значения"
        else
          "Получена строка недопустимой длины"
      }
      case _ => "Что это? Это не строка и не список"
    }
  }

  def toInitials(name: String): String = {
    val regexp = "([A-Z][a-z]+) ([A-Z][a-z]+)"
    if (name != null)
      if ((name.matches(regexp)))
        name.split(" ").map(s => s.take(1)).map(s => s + ".").mkString(" ")
      else
        "Oops, something is wrong"
    else
      "Oops, something is wrong"
  }

  println(toInitials(null))
}

object test_partial extends App {

  /*def chatBotPartial(str: String): Option[String] = {
    case "hello"     => "Hi, I'm Bot"
    case "bye"       => "Bye-bye"
    case "what's up" => "sup-sup"
  }.lift*/

  val chatbot: String => Option[String] = {
    val answer: PartialFunction[String, String] = {
      case "hello"     => "Hi, I'm Bot"
      case "bye"       => "Bye-bye"
      case "what's up" => "sup-sup"
    }
    answer.lift
  }
  scala.io.Source.stdin.getLines().map(chatbot).foreach(println)
}

object count extends App {
  def countNumbers(input: String): Map[Char, Int] = {
    val numbers =
      input.filter(_.isDigit).groupBy(identity)
    println(numbers)
    val quantityNum =
      numbers.mapValues(_.size)
    println(quantityNum)
    quantityNum.toMap
  }

  println(countNumbers("9-876-543-21-09"))

}

object version extends App {
  // val a = List(1, 23)
  // val b = a.tail
  def compare(v1: String, v2: String): Int = {
    val v1List: List[Int] = v1.split('.').map(_.toInt).toList
    val v2List: List[Int] = v2.split('.').map(_.toInt).toList
    def loop(a1: List[Int], a2: List[Int]): Int = {
      (a1, a2) match {
        case (a +: t1, b +: t2) => {
          if (a > b) 1
          else if (a < b) -1
          else loop(t1, t2)
        }
        case (Nil, Nil) => 0
        case (l1 @ Nil, a +: t) =>
          if (a > 0) -1
          else loop(l1, t)
        case (a +: t, l1 @ Nil) =>
          if (a > 0) 1
          else loop(t, l1)
      }
    }
    loop(v1List, v2List)
  }
  def multiplyByTwo(a: Int): Int = a * 2

  val result = multiplyByTwo {
    val aList1 = List("a", "b")
    val aList2 = List("c", "d")

    val aList = aList1 ++: aList2 :+ "p"

    val r1 = aList.zipWithIndex.filter(_._2 != 0)

    val r2 = r1.map(_._1)
    println(r2)
    val r3 = r2.length
    r3

  }

  // println(result)

}

object dubs extends App {

  def duplicate(someList: List[Any], numDups: Int): List[Any] =
    someList.flatMap(s => List.fill(numDups)(s))

}

object chars extends App {

  def countChars(s: String): List[(Char, Int)] = {
    s.toLowerCase
      .foldLeft(Map[Char, Int]().withDefaultValue(0)) { case (acc, letter) =>
        acc + (letter -> (1 + acc(letter)))

      }
      .toList
      .sortBy(s => s._2)
  }
  println(countChars("None"))
}

object practice1 extends App {

  val config: Map[String, String] = Map("host" -> null, "port" -> "port")

  case class Connection(host: String, port: String) {
    def connect: String = "Connected"

  }

  object Connection {
    def apply(host: String, port: String): Option[Connection] =
      if ((host != null) && (port != null)) {
        Some(new Connection(host, port))
      } else { println("Error"); None }
  }

  /*val forConnectionStatus = for {
    host <- config.get("host")
    port <- config.get("port")
    connection <- Connection(host, port)
  } yield connection.connect

  // println(forConnectionStatus)
  forConnectionStatus.foreach(println)*/

  def add(k: String, v: String): Map[String, Set[String]] =
    Map.empty + (k -> Set(v))
}

object locations extends App {

  def add(
      network: Map[String, Set[String]],
      location: String
  ): Map[String, Set[String]] = network + (location -> Set())

  def connect(
      network: Map[String, Set[String]],
      pointA: String,
      pointB: String
  ): Map[String, Set[String]] = {

    network + (pointB -> (network.getOrElse(
      pointB,
      Set()
    ) + pointA)) + (pointA -> (network.getOrElse(pointA, Set()) + pointB))

  }

  def disconnect(
      network: Map[String, Set[String]],
      pointA: String,
      pointB: String
  ): Map[String, Set[String]] = {
    network + (pointB -> (network.getOrElse(
      pointB,
      Set()
    ) - pointA)) + (pointA -> (network.getOrElse(pointA, Set()) - pointB))
  }

  def remove(
      network: Map[String, Set[String]],
      location: String
  ): Map[String, Set[String]] = {
    def loop(
        destinations: Set[String],
        acc: Map[String, Set[String]]
    ): Map[String, Set[String]] =
      if (destinations.isEmpty) acc
      else loop(destinations.tail, disconnect(acc, location, destinations.head))
    val disconnected = loop(network(location), network)
    disconnected - location

  }

  def nFlights(
      network: Map[String, Set[String]],
      location: String
  ): Int = network.getOrElse(location, Set()).size

  def mostFlights(network: Map[String, Set[String]]): String =
    network.maxBy(f => f._2.size)._1

  def isConnected(
      network: Map[String, Set[String]],
      pointA: String,
      pointB: String
  ): Boolean = true

  def nLocationsWithNoFlights(network: Map[String, Set[String]]): Int = 0

}
