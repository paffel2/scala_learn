package stepik8

import scala.runtime.RichInt
import scala.annotation.tailrec

object test11 extends App {

  final case class Pair[T, S](first: T, second: S) {
    def swap: Pair[S, T] = Pair(second, first)
  }
  val pair = Pair(123, "Oleg").swap
  println(pair.first == "Oleg")
  println(pair.second == 123)
}

object test12 extends App {
  case class Box[A](val content: A)

  abstract class Animal {
    def name: String
  }

  case class Cat(name: String) extends Animal
  case class Dog(name: String) extends Animal

  val myAnimal: Animal = Cat("Felix")

  val myAnimalBox: Box[Animal] = Box(Cat("Felix"))

  val myCatBox: Box[Cat] = Box[Cat](Cat("Felix"))

  val myAnimalBox2: Box[Cat] = myCatBox

}

object test21 extends App {

  class A(val value: String)

  case class B(override val value: String) extends A(value)

  val objB = B("It is a B.value")
  val objA = new A("It is a A.value")

  def methodPrint(f: FunctionPrint[B], obj: B) = {
    f(obj)
  }

  class FunctionPrint[-T <: A](prefix: String) {
    def apply(t: T): Unit = println(prefix + " " + t.value)
  }

  object FunctionPrint {
    def apply[T <: A](prefix: String) = new FunctionPrint[T](prefix)
  }

  val printA = FunctionPrint[A]("A-value")
  val printB = FunctionPrint[B]("B-value")

  methodPrint(printA, objB)
}

object test22 extends App {

  class Person(val name: String)

  class Student(name: String, val course: Int) extends Person(name)

  class Pair[+T](val first: T, val second: T) {
    def replaceFirst[N >: T](newValue: N): Pair[N] = {
      new Pair(newValue, second)
    }
  }

  def printNames(pair: Pair[Person]): Unit = {
    println("1: " + pair.first.name + "  2: " + pair.second.name)
  }

  val pair = new Pair(new Student("Pavel", 1), new Student("Oleg", 5))
  printNames(pair.replaceFirst(new Person("Oliver")))
}

object test31 extends App {

  trait Vect extends Any {
    type Item
    def length: Int
    def get(index: Int): Item
    def set(index: Int, item: Item): Vect.Aux[Item]
  }

  object Vect {
    type Aux[I] = Vect { type Item = I }
  }

  final case class StringVect(str: String) extends AnyVal with Vect {
    type Item = Char
    def length = str.length
    def get(index: Int) = str.charAt(index)
    def set(index: Int, item: Char): Vect.Aux[Char] = StringVect(
      str.updated(index, item)
    )
  }

  final case class BoolVect64(values: Long) extends AnyVal with Vect {
    type Item = Boolean
    def length = 64
    def get(index: Int) = (values & (1L << index)) != 0L
    def set(index: Int, item: Boolean): Vect.Aux[Boolean] = {
      if (item)
        BoolVect64(values & (~(1L << index)) | (1L << index))
      else
        BoolVect64(values & (~(1L << index)))
    }
  }

  final case class BoolVect8(values: Byte) extends AnyVal with Vect {
    type Item = Boolean
    def length = 8
    def get(index: Int) = ((values & (1 << index)) & 0xff) != 0
    def set(index: Int, item: Boolean): Vect.Aux[Boolean] = {
      if (item)
        BoolVect8(
          (values & ((~(1 << index)) & 0xff) | (1 << index) & 0xff).toByte
        )
      else
        BoolVect8((values & ((~(1 << index)) & 0xff)).toByte)
    }
  }

  def toList(vect: Vect): List[vect.Item] = {
    @tailrec
    def toList(list: List[vect.Item], index: Int): List[vect.Item] = {
      if (index == vect.length)
        list
      else
        toList(list :+ vect.get(index), index + 1)
    }

    toList(List.empty, 0)
  }

  val a = BoolVect8(126)
  val b: Byte = 127
  println(a.get(6))

}
