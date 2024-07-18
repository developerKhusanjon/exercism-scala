import scala.collection.immutable.ListMap

class School {
  type DB = Map[Int, Seq[String]]

  private var _db: DB = Map.empty

  def add(name: String, g: Int) = _db = _db.updated(g, grade(g) :+ name)

  def db: DB = _db

  def grade(g: Int): Seq[String] = _db.getOrElse(g, Seq.empty)

  def sorted: DB = Map(_db.toSeq.sortBy(_._1): _*).map(m => (m._1 -> m._2.sorted))
}
