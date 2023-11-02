
package stepik4

import scala.annotation.tailrec

object test11 extends App {
  def fibs(num: Int): Int = {
    if (num == 1) 1
    else if (num == 2) 1
    else fibs(num - 1) + fibs(num - 2)
  }
  println(fibs(10))
}


object test12 extends App {

  def sendGift(current_amount: Int, gift_amount: Int): Int = {
    if (current_amount >= 500)
      current_amount + gift_amount
    else
      current_amount
  }

  def plus(x:Int, y:Int) = x + y
  val pl: (Int, Int) => Int = plus
}


object test13 extends  App{
    def searchInArray(cond: Int => Boolean, a: Int): Boolean = {
        cond(a)

    }

    val searchOdd = searchInArray((s => (s % 2) == 1),_)
}



object test31 extends  App{
    def ifThenElse[A](cond: Boolean, t: A, e: A): A =
      if (cond) t else e

    val a: Int = 13
    val b: Int = 42

    print(ifThenElse[Int](false,a,b))
}

object test32 extends App{
  

  @tailrec
  def fibs(n:Int,current_number: Int = 1, f1: BigInt = 0, f2: BigInt = 1): BigInt = {
    if (current_number == n)
       f2
    else
      //print(s"f1=$f1; f2=$f2; current_number=$current_number")
      //readLine()
      fibs(n,current_number+1,f2,f2+f1)
  }

  println(fibs(100))
}


object test33 extends App{
  


def middle[A](data: Iterable[A]): A = {
  val mid: Int = data.size / 2
  data.toList(mid)
  }
require(middle("Scala") == 'a')
require(middle(Seq(1, 7, 5, 9)) == 5)
}