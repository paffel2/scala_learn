object PrintableLibrary extends App {

  trait Printable[A] {
    def format(input: A): String
  }

  case class Cat(name: String, age: Int, color: String)

  object PrintableInstances {
    implicit val formatString: Printable[String] = {
      new Printable[String] {
        def format(input: String) = input
      }
    }

    implicit val formatInt: Printable[Int] = {
      new Printable[Int] {
        def format(input: Int) = input.toString()
      }
    }

    implicit val formatCat: Printable[Cat] = {
      new Printable[Cat] {
        def format(input: Cat) =
          s"${input.name} is a ${input.age} year-old ${input.color} cat."
      }
    }
  }

  object Printable {
    def format[A](input: A)(implicit f: Printable[A]): String = f.format(input)

    def printPrintable[A](input: A)(implicit f: Printable[A]): Unit = println(
      format(
        input
      )
    )

  }

  object PrintableSyntax {
    implicit class PrintableOps[A](input: A) {
      def format(implicit f: Printable[A]): String = f.format(input)
      def printPrintable(implicit f: Printable[A]): Unit = println(input.format)
    }
  }

  import PrintableInstances._

  /*println(Printable.format(123))
  Printable.printPrintable("abc")
  Printable.printPrintable(Cat("Tessi", 1, "Red"))
   */

  import PrintableSyntax._

  (Cat("Tessi", 1, "Red")).printPrintable

}
