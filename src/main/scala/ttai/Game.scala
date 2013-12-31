package ttai

object Game {
  def apply(whoseTurn: Player.Value, handForBlue: Hand, handForRed: Hand): Game = {
    val emptyCells = for {
      row <- (0 until 3)
      col <- (0 until 3)
    } yield Cell(None, row, col)

    val board = Board(emptyCells.toList)
    Game(board, whoseTurn, handForBlue, handForRed)
  }
}

case class Game(board: Board,
                whoseTurn: Player.Value,
                handForBlue: Hand,
                handForRed: Hand) {

  def playCard(card: Card, row: Int, col: Int) = {
    val updatedHandForBlue = if (whoseTurn == Player.Blue) handForBlue.withoutCard(card) else handForBlue
    val updatedHandForRed  = if (whoseTurn == Player.Red) handForRed.withoutCard(card) else handForRed

    // todo: check that the cell does not already have a card
    val updatedBoard = placeCardToBoard(card, row ,col)

    val nextTurn = opposingPlayer
    copy(board = updatedBoard, whoseTurn = nextTurn, handForBlue = updatedHandForBlue, handForRed = updatedHandForRed)
  }

  import Direction._

  private def placeCardToBoard(card: Card, row: Int, col: Int) = {
    def getCellOn(direction: Direction.Value) = direction match {
      case Top    => board.get(row-1, col)
      case Bottom => board.get(row+1, col)
      case Left   => board.get(row, col-1)
      case Right  => board.get(row, col+1)
      case _      => None
    }

    def cellOwnedByOpposingPlayer(cell: Cell) = cell.card match {
      case Some(CardWithOwner(_, owner)) => owner == opposingPlayer
      case _ => false
    }

    val cellToPlaceCardOn = board(row, col).placeCard(card, whoseTurn)

    val oppositeCardData = Direction.values.toList
      .map(dir                         => (dir, getCellOn(dir)))
      .filter { case (_, cellOpt)      => cellOpt.isDefined }
      .filter { case (_, cellOpt)      => cellOwnedByOpposingPlayer(cellOpt.get) }
      .map    { case (dir, cellOpt)    => (dir, cellOpt.get) }
      .map    { case (dir, cell)       => (dir, cell, cell.card.get.card )}
      .map    { case (dir, cell, card) => (dir, cell, card)}

    val cellsToFlip = oppositeCardData.map { case (dir, oppositeCell, oppositeCard) =>
      val oppositeRank = oppositeCard.oppositeRank(dir)
      val rank = card.rank(dir)

      // todo: elemental
      // todo: plus
      // todo: same

      if (rank > oppositeRank) {
        // the placed card flips the opposite card
        oppositeCell.flip
      } else {
        oppositeCell
      }
    }.toList

    val cellsToReplace = cellToPlaceCardOn :: cellsToFlip
    cellsToReplace.foldLeft(board) { (board, cell) =>
      board.replaceCell(cell)
    }
  }

  def opposingPlayer = {
    if (whoseTurn == Player.Blue)
      Player.Red
    else
      Player.Blue
  }
}

