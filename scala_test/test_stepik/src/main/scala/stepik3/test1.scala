package stepik3

import scala.io.StdIn._
object test1 extends App {
  

    val message: String = "something3"

    val message2: AnyRef = message

    val math_message: Unit = Math.cos(_)

    println(message2)

    println(math_message)


}

object test11 extends App{

    def normalDistribution(mu: Double, sigma: Double, x: Double): Double = {

        (1 / (sigma * Math.sqrt(2*Math.PI)))*Math.exp(-(Math.pow((x-mu),2))/(2*sigma*sigma))

    } 
}

object test21 extends App{

    val x = 10.5 //15
    println(x)
}

object test22 extends App{

    def crispsWeight(weight: BigDecimal, potatoWaterRatio: Double, crispsWaterRatio: Double): BigDecimal = {

        val weightPatatoOnly: BigDecimal = weight * (1 - potatoWaterRatio)
        val wightChips: BigDecimal = weightPatatoOnly / (1 - crispsWaterRatio)
        wightChips.setScale(5,BigDecimal.RoundingMode.HALF_UP)
    }   

    val result: BigDecimal = crispsWeight(90.0,0.9,0.1)
    println(result)
}

object test23 extends App {

    print("Enter int number:")
    var someNumber: Int = readInt()
    var counter: Int = 0
    while (someNumber != 0) {
        val mod: Int = someNumber % 2
        if (mod == 1)
            counter += 1
        someNumber = someNumber / 2

    }
    print(counter)
}

object test31 extends App{

    //val s3 = "bar"; val s1 = "foo" + s3; val s2 = "foo" + s3; println(s1 == s2)
    //val s1 = "foo"; val s2 = "foo"; println(s1 == s2)
    //val s3 = "bar"; val s1 = "foo" + s3; val s2 = "foo" + s3; println(s1 eq s2)
    val s1 = "foo"; val s2 = "foo"; println(s1 eq s2)
}

object test32 extends App{

    def isCapital(word: String, pos:Int): Boolean = {
        word.charAt(pos).isUpper
    }

    println(isCapital("Scala",0))
}

object test41 extends App {

    val s1 = 
        """|one
           |two
           |three
        """//.stripMargin
    println(s1)
}

object test42 extends App {

    val numbers = readLine().split(" ")
    val startIndex: Int = numbers(0).toInt
    val endIndex: Int = numbers(1).toInt
    val str: String = readLine()
    val newStr = str.substring(startIndex,endIndex+1)
    val insert = newStr.reverse
    println(str.replaceFirst(newStr,insert))
}

object test43 extends App {

    val str:String = readLine()
    val result = {
        val regex = "[a-z_]+".r
        val start: Char = str(0)
        val end: Char = str.last

        val all_matches = regex.findAllIn(str).toList

        if (all_matches.length != 1)
            false
        else if (!(start.isLetter) || !(end.isLetter))
                false
        else if (str.contains("__"))
            false
        else
            true
    }
    println(result)

    
}