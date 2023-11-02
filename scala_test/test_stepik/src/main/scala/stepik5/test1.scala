package stepik5

import scala.io.StdIn
import scala.util.matching.Regex
object test1 extends App{


    val n = readInt()
    for {i <- 1 until n
         j <- 1 until n if BigInt(i).gcd(BigInt(j)) == 1}
        println(s"$i $j")
}


object test21 extends App{

    case class Pet(name: String, says:String)

    val pet = Pet("Tessa","meow")
    val regex = "[01]+".r
    val kind = pet match {
        case Pet("Rex",_) => "dog"
        case Pet(_,"nya"|"meow") => "cat"
        case Pet(_,regex(says)) => "robot"
        case _ => "unknown"
    }

    print(kind)
}


object test22 extends App{

    val nameAndEmailRegex = "([a-zA-Z]+) \\w+@(\\w+\\.\\w+)".r
    val nameRegex = "([a-zA-Z]+)".r
    val emailRegex = "\\w+@(\\w+\\.\\w+)".r

    val input: List[String] = List("oleg", "oleg@email.com")

    //println(input)

    val result = input match {
        case List(nameAndEmailRegex(name,email),tail@_*) => name + " " + email //.dropWhile(s => s != '@').tail
        case List(nameRegex(name),emailRegex(email),tail@_*) => name + " " + email //.dropWhile(s => s != '@').tail
        case _ => "invalid"
    }

    println(result)

    val r = raw"([a-zA-Z]+)\s\w+@(\w+\.\w+)(?:\n\w+)*".r
    val result1 = input.mkString("\n") match {
        case r(name, email) => s"$name $email"
        case _ => "invalid"
                }
    println(result1)

}

object test31 extends App{
    val divide10: PartialFunction[Int,Int] = {
        case 1 => 10
        case 2 => 5
        case 5 => 2
        case 10 => 1
    }

    print(divide10.isDefinedAt(10))
}

object test32 extends App{
    val log: PartialFunction[Double,Double] = {
        case x if (x > 0) => Math.log(x)
        //case _ => Double.NaN
    }
}

object test33 extends App{

    case class Jar(name: String, value: Int, price: Double) 
    val discount: PartialFunction[Jar, String] = {
        case Jar(name,value,price) if ((10 >= value) && (value >= 5)) => s"$name ${price*0.05}"
        case Jar(name,value,price) if (value  > 10) => s"$name ${price*0.1}"
    }
    val jars = List(Jar("Морской синий 6л", 6, 3000), Jar("Огненно-красный 12л", 12, 5000))
    jars.collect(discount)


}


object test41 extends App{

    def swap3(tuple: (Int,Int,Int)):(Int,Int,Int) = (tuple._3,tuple._2,tuple._1)


}

object test51 extends App{

    def divide(x:Int, y:Int): Option[Int] ={

        if (y == 0) None
        else Some(x / y)
    }

    print(divide(5,1).filter(x => x == 4))
}

object test52 extends App{

    def foo(list:List[Int]):Int = {

        list.find(x => x % 3 == 0).map(x => x * 2).getOrElse(0)
}

}

object test53 extends App{

    def foo(list:List[Int]):Int = {

        list.find(x => x % 3 == 0).map(x => x * 2).getOrElse(0)
}
    
}

object test54 extends App{

    def divide(p: (Int, Int))(q: (Int, Int)): Either[String, (Int, Int)] = {

        if ((q._1 == 0) || (p._2 == 0) || (q._2 == 0))
            Left("Zero divisor")
        else if ((p._1 >= p._2) || (q._1 >= q._2) ) Left("Invalid input")
        else {
                val up = p._1 * q._2
                val down = p._2 * q._1
                if (up >= down)
                    Left("Improper result")
                else
                    {
                        val del = BigInt(up).gcd(BigInt(down)).toInt
                        Right((up/del,down/del))
                    }
            
        }
    }

    
}

object test55 extends App{

    def foo(options: List[Option[Int]]): List[Int] = options.collect{
        case Some(i) => i
    }
    
}