case class Robot(bearing: Bearing.Value, coordinates: (Int, Int)) {

  def simulate(path: String): Robot = path.foldLeft(this)(
    (robot, letter) => letter match {
      case 'L' => robot.turnLeft
      case 'R' => robot.turnRight
      case 'A' => robot.advance
      case _ => robot
    }
  )

  def advance: Robot = bearing match {
    case Bearing.East => Robot(bearing, (coordinates._1 + 1, coordinates._2))
    case Bearing.West => Robot(bearing, (coordinates._1 - 1, coordinates._2))
    case Bearing.North => Robot(bearing, (coordinates._1, coordinates._2 + 1))
    case Bearing.South => Robot(bearing, (coordinates._1, coordinates._2 - 1))
  }

  def turnLeft: Robot = bearing match {
    case Bearing.East => Robot(Bearing.North, coordinates)
    case Bearing.West => Robot(Bearing.South, coordinates)
    case Bearing.North => Robot(Bearing.West, coordinates)
    case Bearing.South => Robot(Bearing.East, coordinates)
  }

  def turnRight: Robot = bearing match {
    case Bearing.East => Robot(Bearing.South, coordinates)
    case Bearing.West => Robot(Bearing.North, coordinates)
    case Bearing.North => Robot(Bearing.East, coordinates)
    case Bearing.South => Robot(Bearing.West, coordinates)
  }
}

object Bearing extends Enumeration {
  val South, North, East, West = Value
}
