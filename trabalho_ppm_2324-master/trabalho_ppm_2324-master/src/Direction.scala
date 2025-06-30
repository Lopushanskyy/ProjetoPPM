
object Direction extends Enumeration {
  type Direction = Value
  val North, South, East, West,
  NorthEast, NorthWest, SouthEast, SouthWest = Value
  type Coord2D = (Int, Int)

  def incrementDirection(direction: Direction, pos: Coord2D): Coord2D = {
    direction match {
      case North => (pos._1 - 1, pos._2)
      case South => (pos._1 + 1, pos._2)
      case East => (pos._1, pos._2 + 1)
      case West => (pos._1, pos._2 - 1)
      case NorthEast => (pos._1 - 1, pos._2 + 1)
      case NorthWest => (pos._1 - 1, pos._2 - 1)
      case SouthEast => (pos._1 + 1, pos._2 + 1)
      case SouthWest => (pos._1 + 1, pos._2 - 1)
    }
  }

  def parseDirection(input: String): Value = {
    input.toLowerCase match {
      case "north" => North
      case "south" => South
      case "east" => East
      case "west" => West
      case "northeast" => NorthEast
      case "northwest" => NorthWest
      case "southeast" => SouthEast
      case "southwest" => SouthWest
    }
  }

}
