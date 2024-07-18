object PascalsTriangle {
  def rows(count: Int): List[_] = {
    if (count <= 0) List()
    else (0 to count)
      .foldLeft(List[List[Int]](List()))((l, _) => l.appended(pascalRow(l.last)))
  }

  def pascalRow(old: List[Int]): List[Int] =
    old.foldLeft((0, List[Int]()))((tuple, elem) => (elem, tuple._2 ++ List(tuple._1 + elem)))._2

  def main(args: Array[String]): Unit = {
//    println(rows())
  }
}