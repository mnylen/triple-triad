package ttai

object Game {
  def apply(whoseTurn: Player.Value, handForBlue: Hand, handForRed: Hand, rules: Set[Rule.Value] = Set.empty): Game = {
    val emptyCells = for {
      row <- (0 until 3)
      col <- (0 until 3)
    } yield Cell(None, row, col)

    val board = Board(emptyCells.toList)
    Game(board, whoseTurn, handForBlue, handForRed, rules)
  }
}

case class Game(board: Board,
                whoseTurn: Player.Value,
                handForBlue: Hand,
                handForRed: Hand,
                rules: Set[Rule.Value]) {

  def playCard(card: Card, row: Int, col: Int) = {
    val updatedHandForBlue = if (whoseTurn == Player.Blue) handForBlue.withoutCard(card) else handForBlue
    val updatedHandForRed  = if (whoseTurn == Player.Red) handForRed.withoutCard(card) else handForRed

    // todo: check that the cell does not already have a card
    val updatedBoard = placeCardToBoard(applyPlusRuleIfInEffect(card, row, col), card, row ,col)

    val nextTurn = opposingPlayer
    copy(board = updatedBoard, whoseTurn = nextTurn, handForBlue = updatedHandForBlue, handForRed = updatedHandForRed)
  }

  private def placeCardToBoard(board: Board, card: Card, row: Int, col: Int) = {
    val getCellOn = board.getRelative(row, col)(_)

    def cellOwnedByOpposingPlayer(cell: Cell) = cell.card match {
      case Some(CardWithOwner(_, owner)) => owner == opposingPlayer
      case _ => false
    }

    val cellToPlaceCardOn = board(row, col).placeCard(card, whoseTurn)
    val cardAdjustedForElement = getCardWithRanksAdjustedForElement(cellToPlaceCardOn)

    val oppositeCardData = Direction.values.toList
      .map(dir                         => (dir, getCellOn(dir)))
      .filter { case (_, cellOpt)      => cellOpt.isDefined }
      .filter { case (_, cellOpt)      => cellOwnedByOpposingPlayer(cellOpt.get) }
      .map    { case (dir, cellOpt)    => (dir, cellOpt.get) }
      .map    { case (dir, cell)       => (dir, cell, getCardWithRanksAdjustedForElement(cell)) }

    val cellsToFlip = oppositeCardData.map { case (dir, oppositeCell, oppositeCard) =>
      val oppositeRank = oppositeCard.oppositeRank(dir)
      val rank = cardAdjustedForElement.rank(dir)

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

  private def applyPlusRuleIfInEffect(card: Card, row: Int, col: Int) = {
    if (rules.contains(Rule.Plus)) {
      applyPlusRule(board, card, row, col)
    } else {
      board
    }
  }

  private def applyPlusRule(board: Board, card: Card, row: Int, col: Int) = {
    val validDirections = Direction.values.filter(dir => thereIsCardInDirection(board, row, col, dir)).toSet
    val directionCombos = validDirections.subsets(2)

    val ruleAppliesInDirections = directionCombos
      .filter(dirs => plusRuleApplies(board, card, row, col, dirs))
      .flatten

    if (ruleAppliesInDirections.nonEmpty) {
      validDirections.foldLeft(board) { (board, dir) =>
        val adjacentCell = board.getRelative(row, col)(dir).get

        // to apply combo rule, we re-play the card as current player and apply normal flip rules
        if (adjacentCell.isOwnedBy(opposingPlayer)) {
          val boardWithAdjacentFlipped = board.replaceCell(adjacentCell.flip)
          placeCardToBoard(boardWithAdjacentFlipped, adjacentCell.card.get.card, adjacentCell.row, adjacentCell.col)
        } else {
          board
        }
      }
    } else {
      board
    }
  }

  private def plusRuleApplies(board: Board, card: Card, row: Int, col: Int, directions: Set[Direction.Value]) = {
    val sum = directions.map(dir => {
      val adjacentCell = board.getRelative(row, col)(dir).get
      val adjacentCard = adjacentCell.card.get.card
      adjacentCard.oppositeRank(dir) + card.rank(dir)
    })

    if (sum.size == 1) true else false
  }

  private def thereIsCardInDirection(board: Board, row: Int, col: Int, direction: Direction.Value) = {
    val cell = board.getRelative(row, col)(direction)
    cell match {
      case Some(Cell(Some(_), _, _, _)) => true
      case _ => false
    }
  }

  def getCardWithRanksAdjustedForElement(cell: Cell) = {
    val card = cell.card.get.card

    if (rules.contains(Rule.Elemental) && cell.element != Element.None) {
      if (card.element == cell.element) {
        card.copy(rankTop = card.rankTop + 1, rankLeft = card.rankLeft + 1, rankRight = card.rankRight + 1, rankBottom = card.rankBottom + 1)
      } else {
        card.copy(rankTop = card.rankTop - 1, rankLeft = card.rankLeft - 1, rankRight = card.rankRight - 1, rankBottom = card.rankBottom - 1)
      }
    } else {
      card
    }
  }

  def opposingPlayer = {
    if (whoseTurn == Player.Blue) Player.Red else Player.Blue
  }

  def isOver = {
    (handForBlue.cards.size + handForRed.cards.size) == 1
  }
}

