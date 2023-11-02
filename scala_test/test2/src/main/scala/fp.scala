object fptest1 extends App {

  val progLanguages = List("java", "scala", "python")
  val lngAbbrev = List("JA", "SCA", "PY")

  val out = lngAbbrev.zip(progLanguages)

  val sampleTuple = new Tuple2(2, "Hello, World")
  val nums1 = List(1, 2, 3)
  val nums2 = List(4, 6, 7)
  val nums3 = List(10, 100, 1000)
  println(
    for {
      a <- nums1
      b <- nums2 if b % 2 == 1
      c <- nums3
    } yield (a + b) * c
  )
}
