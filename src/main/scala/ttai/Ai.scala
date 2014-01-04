package ttai

object Ai {
  def getNextMove(game: Game) = {
    val freeCellsLeft = game.board.cells.count(_.isEmpty)
    val maxDepth = if (freeCellsLeft == 9) 4 else if (freeCellsLeft == 8) 5 else freeCellsLeft
    val (_, move) = negamax(game, maxDepth)
    move
  }

  private def negamax(game: Game, depthLeft: Int): (Int, Option[PlayCardTo]) = {
    val factor = if (game.whoseTurn == Player.Blue) +1 else -1

    if (depthLeft == 0 || game.isOver) {
      val score = factor * BoardValuator.calculateGameValue(game)
      (score, None)
    } else {
      val moves = possibleMoves(game)
      val initialResult = (Int.MinValue, None.asInstanceOf[Option[PlayCardTo]])

      moves.par.foldLeft(initialResult) { case (bestResult @ (bestValue, _), move) =>
        val gameAfterMove = game.playCard(move.card, move.row, move.col)

        val result = negamax(gameAfterMove, depthLeft - 1)
        val value  = -1 * result._1

        if (value > bestValue) {
          (value, Some(move))
        } else {
          bestResult
        }
      }
    }
  }

  private def possibleMoves(game: Game) = {
    val possibleCards = (if (game.whoseTurn == Player.Blue) game.handForBlue else game.handForRed).cards
    val possibleCells = game.board.cells.filter(_.isEmpty)

    for {
      cell <- possibleCells
      card <- possibleCards
    } yield PlayCardTo(card, cell.row, cell.col)
  }
}

case class PlayCardTo(card: Card, row: Int, col: Int)

object BoardValuator {
  val ValueOfOwnCard = 10
  val ValueOfOpponentsCard = -10
  val ValueOfEmptyCell = 0
  val ValueOfLoss = -100
  val ValueOfDraw = -30
  val ValueOfWin  = 100

  def calculateGameValue(game: Game) = {
    if (game.isOver) {
      val numberOfBlueCards = game.board.cells.count(_.isOwnedBy(Player.Blue))
      val numberOfRedCards  = game.board.cells.count(_.isOwnedBy(Player.Red))

      if (numberOfBlueCards == numberOfRedCards) {
        ValueOfDraw
      } else if (numberOfBlueCards > numberOfRedCards) {
        ValueOfWin
      } else {
        ValueOfLoss
      }
    } else {
      // applicable for first & second turns
      game.board.cells.foldLeft(0) { case (currentValue, cell) =>
        if (cell.isEmpty) {
          currentValue + ValueOfEmptyCell
        } else if (cell.isOwnedBy(Player.Blue)) {
          currentValue + ValueOfOwnCard
        } else {
          currentValue - ValueOfOpponentsCard
        }
      }
    }
  }
}