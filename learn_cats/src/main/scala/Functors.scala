import cats.implicits._
import cats._

object Functors extends App {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)
    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }

  object functorInstance {
    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {

      def map[A, B](value: Tree[A])(func: A => B): Tree[B] = value match {
        case Leaf(a)             => Leaf(func(a))
        case Branch(left, right) => Branch(map(left)(func), map(right)(func))
      }
    }
  }

  import functorInstance._

  // Branch(Leaf(10), Leaf(20)).map(_ * 2) // в таком выражении, компилятор не может найти экземляр для Branch
  val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
  println(
    tree.map(_ * 2)
  ) // для переменной tree мы уточнили тип, потому все работает, можно воспользоваться объектом компаньеном, для создания
  // умных конструкторов

  trait Printable[A] { self =>
    def format(value: A): String
    def contramap[B](func: B => A): Printable[B] = new Printable[B] {
      def format(value: B): String = self.format(func(value))
    }

  }
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        s"'${value}'"
    }
  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }
  // println(format("hello"))
// res2: String = "'hello'"
  // println(format(true))
// res3: String = "yes"

  final case class Box[A](value: A)

  implicit def printableBox[A](implicit p: Printable[A]): Printable[Box[A]] =
    new Printable[Box[A]] {
      def format(box: Box[A]): String = p.format(box.value)
    }

  println(format(Box("hello world")))
// res4: String = "'hello world'"
  println(format(Box(true)))
  // println(format(Box(123))) не работает так нет Printable для Int
// res5: String = "yes"

  trait Codec[A] { self =>
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = {
      new Codec[B] {
        def encode(value: B): String =
          self.encode(enc(value))
        def decode(value: String): B =
          dec(self.decode(value))
      }
    }
  }
}
