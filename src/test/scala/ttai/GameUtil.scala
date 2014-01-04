package ttai

object GameUtil {
  def parseGame(gameState: String) = {
    val rows = gameState.split("\n")

    val parsedCells = rows.map(_.split(",")).zipWithIndex.map { case (cells, row) =>
      cells.zipWithIndex.map { case (cell, col) =>
        if (cell == "EMPTY{_}") {
          Cell(None, row, col)
        } else {
          val owner = Player.forSymbol(cell.charAt(0))
          val rankTop = parseRank(cell.charAt(1))
          val rankLeft = parseRank(cell.charAt(2))
          val rankRight = parseRank(cell.charAt(3))
          val rankBottom = parseRank(cell.charAt(4))
          val element = Element.forSymbol(cell.charAt(6))

          val card = Card(rankTop, rankLeft, rankRight, rankBottom, element)
          Cell(Some(CardWithOwner(card, owner)), row, col)
        }
      }
    }.flatten

    val board = Board(parsedCells.toList)
    val game = Game(board, Player.Blue, Hand.empty, Hand.empty, Set.empty)

    game
  }

  private def parseRank(rank: Char) = {
    if (rank == 'A') {
      10
    } else {
      rank.toString.toInt
    }
  }
}
