package stepik6

import scala.collection.mutable._
import scala.io.StdIn
object test11 extends App{
  
    val strings = Buffer[String]()

    strings += "aaa"
    strings += "bbb"
    strings += "ccc"
    strings += "ddd"

    strings.map(s => println(s))
}

object test12 extends App{
  
    val ints: List[Int] = List(0,1,1,0,1,0,0,1,1,1,0,1,0)

    val zeros: List[Int] = ints.filter{case i: Int => i == 0}
    val ones: List[Int] = ints.filter{case i: Int => i == 1}

    println(zeros.mkString(","))
    println(ones.mkString(","))
    

}

object test13 extends App{
    
    val n: Int = readInt()
    val ints = readLine().split(" ").toList.map(_.toInt).sorted
    def kOrder(list: List[Int], k: Int): Int = {
        if (k == 1)
            list.head
        else
            kOrder(list.tail,k-1)
    }

 
    

}

object test21 extends App{
    
    val list = List(1,2,3)
    println(list.apply(1))
    println(list(1))
    //println(list.get(1))
    println(list.take(1))


}

object test22 extends App{
    
    val list = List(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150)
    list.collect{
        case x if ((x < 100) && (x % 4 == 0)) => x / 4
    }.init.foreach(println)


}

object test23 extends App{
    
    val bufferStrings: Stream[String] = Stream.continually(readLine).takeWhile(_ != "END")
    val buffer = bufferStrings.map(s => s.toInt)
    println(buffer)
    var sum = 0
    print(buffer(1))
    for (i <- 0 until buffer.size if i % 2 == 1)
        sum += buffer(i)*2
    println(sum)

}

object test24 extends App{
    
    val points: List[Int] = List(1,3)// точки кроссинговера
    val chr1: List[Char] = List('x', 'x', 'x', 'x', 'x') // первая хромосома
    val chr2: List[Char] = List('y', 'y', 'y', 'y', 'y') // вторая хромосома

    def crossingoverForList(points: List[Int], chr1: List[Char], chr2:List[Char]): (List[Char],List[Char]) =
        {
            def crossingover(point:Int,chr1: List[Char], chr2:List[Char]): (List[Char],List[Char]) = 
            {
                if (point == 0)
                    (chr2, chr1)
                else
                    {
                        val (res1,res2) = crossingover(point-1,chr1.tail,chr2.tail)
                        (chr1.head :: res1, chr2.head :: res2)
                    }
            }

            points match {
                case List() => (chr1,chr2)
                case head :: tail => {
                    val (res1,res2) = crossingover(head, chr1,chr2)
                    crossingoverForList(tail,res1,res2)
                }
            }
        }
    
        val (res1,res2) = crossingoverForList(points,chr1,chr2)
        println(res1.mkString)
        println(res2.mkString)

}
    /*
object test26 extends App{

    final class DiffListImpl[A](listFunc: List[A] => List[A]) extends DiffList[A](listFunc) {
  def prepend(s: List[A]) = s ::: listFunc

  def append(s: List[A]) = ???

  def result = ???
}

}
*/ 