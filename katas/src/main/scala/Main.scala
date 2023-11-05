object Main extends App {

  def alphabetPosition(text: String): String =
    text
      .toLowerCase()
      .toList
      .filter(_.isLetter)
      .map(c => c.toInt - 96)
      .mkString(" ")

  def filterList(list: List[Any]): List[Int] = list.collect({ case x: Int =>
    x
  })

  def nbYear(p0: Int, percent: Double, aug: Int, p: Int): Int = {
    def loop(years: Int = 0, pbefore: Int): Int =
      if (pbefore >= p)
        years
      else {
        val pnew: Int = pbefore + (pbefore * percent * 0.01).toInt + aug
        loop(years + 1, pnew)
      }
    loop(0, p0)

  }

}
