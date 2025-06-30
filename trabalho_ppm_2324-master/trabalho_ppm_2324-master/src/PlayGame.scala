import scala.annotation.tailrec
import scala.io.StdIn

//t8
case class GameState(foundWords: List[String], startTime: Long)
object PlayGame extends App{

  val MAXPLAYTIME = 180
  val (words, positions) = ReadFile.readFileWithWords("src/words.txt")
  val numberOfWords = words.length
  val r = MyRandom(10)
  val s = GameState(List.empty[String], System.currentTimeMillis())

  gameLoop(s,r)
    @tailrec
    def gameLoop(gameState: GameState, rand: MyRandom): Unit = {
      val board = List.fill(5, 5)(' ')
      val zigzag = ZigZag(rand)
      val setBoard = zigzag.setBoardWithWords(board, words, positions)
      val (completeBoard, finalRandom) = zigzag.completeBoard(setBoard)
      zigzag.printBoard(completeBoard)

      println("Enter the word:")
      val word = StdIn.readLine().toUpperCase

      println("Enter the position (row,column):")
      val positionInput = StdIn.readLine()
      val position = {
        val Array(row, column) = positionInput.split(",").map(_.toInt)
        (row, column)
      }

      println("Enter the initial direction:")
      val initialDirection = StdIn.readLine()

      val foundWord = zigzag.play(completeBoard, word, position, Direction.parseDirection(initialDirection))
      println(foundWord)

      if (foundWord && !gameState.foundWords.contains(word)) {
        val newFoundWords = word :: gameState.foundWords
        val gameStateWordFound = gameState.copy(foundWords = newFoundWords)

        if (checkBoard(completeBoard, newFoundWords, words)){
        // if (newFoundWords.length == numberOfWords) {
          println("You found all the correct words!")
          val endTime = System.currentTimeMillis()
          val elapsedTimeInSeconds = ((endTime - gameState.startTime) / 1000.0).toInt
          val points = calculatePoints(elapsedTimeInSeconds)
          println("Points: " + points)
          println("New random Board? (yes/no)")
          val tryAgainInput = StdIn.readLine()

          if (tryAgainInput == "yes") {
            val gameStateNewGame = gameState.copy(foundWords = List.empty[String], startTime = System.currentTimeMillis())
            gameLoop(gameStateNewGame, finalRandom)
          }

        } else {
          println("\nCorrect guess, more words to find\n")
          gameLoop(gameStateWordFound, rand)
        }
      } else if (foundWord && gameState.foundWords.contains(word)) {
        println("\nYou already found this word!\n")
        gameLoop(gameState, rand)
      } else {
        println("\nWrong! Try again\n")
        gameLoop(gameState, rand)
        }
      }

  //t6
  def checkBoard(board: ZigZag.Board, foundWords: List[String], allWords: List[String]): Boolean = {
    val allWordsFound = allWords.foldLeft(true)((acc, word) => acc && foundWords.contains(word))

    val foundWordsValid = foundWords.foldLeft(true) { (acc, word) =>

      val isValidWord = allWords.contains(word)

      val isNotDuplicated = foundWords.count(_ == word) == 1

      acc && isValidWord && isNotDuplicated
    }

    allWordsFound && foundWordsValid
  }



  //t7
  def calculatePoints(elapsedTimeInSeconds: Int): Int = {
    if (elapsedTimeInSeconds > MAXPLAYTIME) 0
    else MAXPLAYTIME - elapsedTimeInSeconds
  }




}
