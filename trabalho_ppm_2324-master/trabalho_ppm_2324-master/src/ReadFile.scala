import Direction.Coord2D
import scala.io.Source

object ReadFile {

  def readFileWithWords(arquivo: String): (List[String], List[List[Coord2D]]) = {
    val linhas = Source.fromFile(arquivo).getLines().toList
    val palavras = linhas.zipWithIndex.collect {
      case (line, index) if index % 2 == 0 => line
    }

    def toCoord2DList(nums: List[Int]): List[Coord2D] = {
      nums.grouped(2).collect {
        case List(x, y) => (x, y)
      }.toList
    }

    val coordenadas = linhas.zipWithIndex.collect {
      case (line, index) if index % 2 != 0 && line.matches("\\d+(,\\d+)+") => toCoord2DList(line.split(",").map(_.toInt).toList)
    }

    (palavras, coordenadas)
  }
}
