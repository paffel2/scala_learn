package stepik7

object test11 extends App {

  class Dog(val name: String) {
    // println(s"$name created")
    def woof = println("woof")
  }

  val dog = new Dog("Rex")
  dog.woof
  println(dog.name)

}

object test12 extends App {

  class Waiter(val name: String, var order: List[String]) {
    println(s"My name $name")
    def giveMe(some: String) = {
      order = order :+ some
      this
    }

    def complete() = {
      println(s"Order: ${order.mkString(",")}")
      order
    }
  }

  val waiter = new Waiter("Иван", List())
  val positions = waiter.giveMe("борщ").giveMe("хлеб").complete()
}

object test21 extends App {

  trait Animal {
    def name: String
    val greeting: String = s"hi $name"
  }

  val animal = new Animal {
    def name = "Dog"
  }

}

object test22 extends App {

  trait StringProcessor {
    def process(input: String): String
  }
  val tokenDeleter = new StringProcessor {
    def process(input: String): String = {
      input.filter(s => !",.:;!".contains(s))
    }
  }

  val toLowerConvertor = new StringProcessor {
    def process(input: String): String = {
      input.toLowerCase()
    }
  }

  val shortener = new StringProcessor {
    def process(input: String): String = {
      if (input.size > 20)
        input.take(20) + "..."
      else
        input
    }
  }
  val test = "This is a Wonderful Test!"

  println(tokenDeleter.process(test))
  println(toLowerConvertor.process(test))
  println(shortener.process("aaa"))

}

object test31 extends App {

  class Point(val x: Double, val y: Double, val z: Double)

  object Point {
    def apply(x: Double, y: Double, z: Double): Point = {
      new Point(x, y, z)
    }

    def average(list: List[Point]): Point = {

      if (list.nonEmpty) {
        val sumPoint =
          list.reduce((p1, p2) => Point(p1.x + p2.x, p1.y + p2.y, p1.z + p2.z))

        val n = list.size
        Point(sumPoint.x / n, sumPoint.y / n, sumPoint.z / n)
      } else
        Point(0, 0, 0)
    }

    def show(point: Point): String = s"${point.x} ${point.y} ${point.z}"
  }

}

object test32 extends App {

  object FacedString {
    def apply(input: String) = s"*_*$input*_*"

    def unapply(arg: String): Option[String] = {
      val regexp = "\\*_\\*(.*)\\*_\\*".r
      arg match {
        case regexp(arg) => Some(arg)
        case _           => None
      }

    }
  }
}

object test41 extends App {
  trait Animal {
    val sound: String
    def voice: Unit = println("voice: " + sound)
  }

  class Cat extends Animal {
    val sound = "Meow!"
  }

  class Dog extends Animal {
    val sound = "Woof!"
  }

  class Fish extends Animal {
    val sound: String = ""
    override def voice: Unit = println("Fishes are silent!")
  }
}

object test42 extends App {
  trait AbstractBank {
    def a: Char
    def b: Char
    def c: Char
    def d: Char
    def e: Char
    def f: Char
    def issueCredit: Unit
  }
  trait BankA extends AbstractBank {
    override val b = 'T'
    override val d = 'R'
    override val f = 'I'
  }

  trait BankB extends AbstractBank {
    override val a = 'E'
    override val f = 'D'
  }

  trait BankC extends AbstractBank {
    override val b = 'C'
    override val d = 'D'
  }

  trait BankD extends AbstractBank {
    override val b = 'C'
    override val c = 'R'
    override val d = 'E'
  }

  trait BankE extends AbstractBank {
    override val b = 'C'
    override val a = 'I'
    override val e = 'T'
  }

  class CreditBank extends BankD with BankE with BankB with BankA with BankC {
    def issueCredit = println(
      "" + b + c + a + d + f + e
    )
  }
  val a = new CreditBank
  a.issueCredit
}

object tst extends App {

  class Dog {
    private[this] val kind = "dog"
    def kindOf = println(kind)
  }

  val a = new Dog
  a.kindOf
}
