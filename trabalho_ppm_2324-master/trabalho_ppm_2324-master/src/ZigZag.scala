import Direction.{Coord2D, Direction}
import scala.annotation.tailrec
import ZigZag.Board


case class ZigZag(rand: MyRandom) {

  def setBoardWithWords(board: Board, words: List[String], positions: List[List[Coord2D]]): Board = ZigZag.setBoardWithWords(board, words, positions)

  def completeBoard(board: Board): (Board, MyRandom) = ZigZag.completeBoard(board, rand, ZigZag.randomChar)

  def play(board: Board, word: String, position: Coord2D, direction: Direction): Boolean = ZigZag.play(board, word, position, direction)

  def printBoard(board: Board) = ZigZag.printBoard(board)

}

object ZigZag {

  type Board = List[List[Char]]

  //T1
  def randomChar(rand: MyRandom): (Char, MyRandom) = {
    val (n, nextRandom) = rand.nextInt
    val lowerBound = 'A'.toInt
    val upperBound = 'Z'.toInt
    val charCode = lowerBound + (n.abs % (upperBound - lowerBound + 1))
    (charCode.toChar, nextRandom)
  }

  def validCoords(board: Board, coord: Coord2D): Boolean = {
    val (row, column) = coord
    if (row >= 0 && row < board.length && column >= 0 && column < board(row).length) true
    else false
  }

  //T2
  def fillOneCell(board: Board, letter: Char, coord: Coord2D): Board = {
    val (row, column) = coord
    if (validCoords(board, coord))
      board.updated(row, board(row).updated(column, letter))
    else
      throw new IllegalArgumentException("Coordinates not valid")
  }


  //T3
  def setBoardWithWords(board: Board, words: List[String], positions: List[List[Coord2D]]): Board = {

    def processWord(b: Board, word: String, coords: List[Coord2D]): Board =
      (word.zip(coords) foldLeft b) { case (accBoard, (char, coord)) =>
        if (validCoords(board, coord))
          fillOneCell(accBoard, char, coord)
        else
          accBoard
      }

    (words zip positions).foldLeft(board) { case (accBoard, (word, coords)) =>
      processWord(accBoard, word, coords)
    }
  }


  //T4
  def completeBoard(board: Board, r: MyRandom, f: MyRandom => (Char, MyRandom)): (Board, MyRandom) = {
    @tailrec
    def loop(board: Board, r: MyRandom, f: MyRandom => (Char, MyRandom), lin: Int, col: Int): (Board, MyRandom) = {
      if (lin >= board.length) {
        (board, r)
      } else if (col >= board(lin).length) {
        loop(board, r, f, lin + 1, 0)
      } else {
        val cel = board(lin)(col)
        if (cel == ' ') {
          val (randomChar, newRandom) = f(r)
          val updatedBoard = fillOneCell(board, randomChar, (lin, col))
          loop(updatedBoard, newRandom, f, lin, col + 1)
        } else {
          loop(board, r, f, lin, col + 1)
        }
      }
    }

    loop(board, r, f, 0, 0)
  }


  //T5
  def play(board: Board, word: String, position: Coord2D, direction: Direction): Boolean = {

    def checkWord(board: Board, remainingWord: List[Char], currentPosition: Coord2D, currentDirection: Direction, isFirstDirection: Boolean): Boolean = {
      remainingWord match {
        case Nil => true
        case head :: tail =>
          if (validCoords(board, currentPosition) && board(currentPosition._1)(currentPosition._2) == head) {

            val nextPosition = Direction.incrementDirection(currentDirection, currentPosition)

            if (isFirstDirection) {
              checkWord(board, tail, nextPosition, currentDirection, isFirstDirection = false)
            } else if (!checkWord(board, tail, nextPosition, currentDirection, isFirstDirection = false))
              Direction.values.exists(dir => dir != currentDirection && checkWord(board, tail, Direction.incrementDirection(dir, currentPosition), dir, isFirstDirection = false))
            else true

          } else false
      }
    }

    checkWord(board, word.toList, position, direction, isFirstDirection = true)
  }






  def printBoard(board: List[List[Char]]): Unit = {
    val result = board.map(_.mkString(" ")).mkString("\n")
    println(result)
    println()
  }

}
