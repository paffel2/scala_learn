object test1 extends App {

  abstract class LogList {
    def last: String
    def previous: LogList
    def isEmpty: Boolean
    def all: String
    def add(msg: String): LogList
  }

  class Log(head: String, tail: LogList) extends LogList {
    def last = head
    def previous: LogList = tail
    def isEmpty: Boolean = false
    def all = this.head + " " + previous.all
    def add(msg: String) = new Log(msg, this)
  }
  object Empty extends LogList {
    def last = throw new NoSuchElementException
    def previous = throw new NoSuchElementException
    def isEmpty = true
    def all = ""
    def add(msg: String) = new Log(msg, this)
  }

}

object test2 extends App {
  abstract class LogList[+A] {
    def last: A
    def previous: LogList[A]
    def isEmpty: Boolean
    def all: String
    def add[B >: A](msg: B): LogList[B]
  }

  class Log[+A](head: A, tail: LogList[A]) extends LogList[A] {
    def last = head
    def previous: LogList[A] = tail
    def isEmpty: Boolean = false
    def all = this.head.toString + " " + previous.all
    override def add[B >: A](msg: B) = new Log[B](msg, this)
  }

  object Empty extends LogList[Nothing] {
    def last = throw new NoSuchElementException
    def previous = throw new NoSuchElementException
    def isEmpty = true
    def all = ""
    def add[B >: Nothing](msg: B) = new Log[B](msg, this)
  }

  val intLogs: LogList[Int] = Empty
  val strLogs: LogList[String] = Empty

  println(intLogs.all)
  println(strLogs.all)

  val notEmptyIntList = intLogs.add(1).add(2).add(3)
  val notEmptyStringList = strLogs.add("a").add("b").add("c")

  println(notEmptyIntList.all)
  println(notEmptyStringList.all)

}

object test5 extends App {

  def someFunc: Int => Function1[Int, Int] =
    new Function1[Int, Function1[Int, Int]] {
      override def apply(x: Int): Function1[Int, Int] =
        new Function1[Int, Int] {
          override def apply(y: Int): Int = x + y
        }
    }
  println(someFunc(1)(4))
}

object test6 extends App {

  def add(x: Int, y: => Int) = x + y
  def multiply(f: () => Int) = f() * 2
  def four: Int = 4
  def two(): Int = 2

  multiply(two)

  multiply(() => 4)

  add(two(), four)

  // multiply(12)

  add(1, four)

  // add(1, () => 2)

  multiply(two _)

  // multiply(four)

  // add(1, two _)

  add(1, (() => 2)())

  add(1, 2)
}
