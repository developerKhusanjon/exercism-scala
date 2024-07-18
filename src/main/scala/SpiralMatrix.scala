object SpiralMatrix {
  def spiralMatrix(n: Int): List[List[Int]] = {
    var result: List[List[Int]] = (1 to n).map(List()).toList

    (1 to n * n).foldLeft((result, 0, 0, 1))((R, e) => R match {
      case (l, x, y, direct) if (direct == 1 && y <= n - 1)
      => if (y == n - 1) (l.updated(x, l(x).updated(y, e)), x + 1, y, 2) else (l.updated(x, l(x).updated(y, e)), x, y + 1, direct)
      case (l, x, y, direct) if (direct == 2 && x <= n - 1)
      => if (x == n - 1) (l.updated(x, l(x).updated(y, e)), x + 1, y, 2) else (l.updated(x, l(x).updated(y, e)), x, y + 1, direct)
//      case (l, x, y) if (x < n) =>
    })._1
  }


}
