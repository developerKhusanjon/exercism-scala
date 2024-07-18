object BinarySearch {
  def find(titles: List[Int], title: Int): Option[Int] = {
    var l = titles
    var index: Int = Math.ceil(l.length / 2).toInt


    while (l.nonEmpty) {
      if (l(index) == title) return Some(index)
      if (l(index) > title) l = l.slice(index + 1, l.length)
      else l = l.slice(0, index)
      index = Math.ceil(l.length / 2).toInt
    }

    None
  }

  def findIndex(l: List[Int], t: Int, index: Int): Option[Int] = {
    if (index == 0 && l.head != t) return None
    if (l(index) == t) Some(index)
    else if (l(index) > t) findIndex(l.slice(index + 1, l.length), t, l.length / 2)
    else findIndex(l.slice(0, index), t, l.length / 2)
  }

  def main(args: Array[String]): Unit = {
    var a: Array[Int] = Array()

    a = Array(1) ++ a
    val b = 5
    (0 to 5).f


    println(a.mkString("Array(", ", ", ")"))
  }
}
