object Etl {
  def transform(scoreMap: Map[Int, Seq[String]]): Map[String, Int] = {
    var resultMap: Map[String, Int] = Map.empty

    for (k <- scoreMap.keys) {
      scoreMap(k).foreach(letter => resultMap = resultMap.updated(letter.toLowerCase(), k))
    }

    resultMap

    scoreMap.flatMap {
      case (score, letters) =>
        letters.map(_.toLowerCase -> score)
    }
  }

  def main(args: Array[String]): Unit = {

  }
}
