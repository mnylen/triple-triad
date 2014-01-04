package ttai

import ttai.Direction._

object Board {
  lazy val empty = Board(
    (for {
      row <- (0 until 3)
      col <- (0 until 3)
    } yield Cell(None, row, col, Element.None)).toList
  )
}

case class Board(cells: List[Cell]) {
  def apply(row: Int, col: Int) = {
    get(row, col).getOrElse(throw new NoSuchElementException(s"No cell found from index ($row, $col)"))
  }

  def get(row: Int, col: Int) = {
    if (row < 0 || col < 0 || row > 2 || col > 2) {
      None
    } else {
      val index = row * 3 + col
      Some(cells(index))
    }
  }

  def getRelative(row: Int, col: Int)(dir: Direction.Value) = dir match {
    case Top    => get(row-1, col)
    case Bottom => get(row+1, col)
    case Left   => get(row, col-1)
    case Right  => get(row, col+1)
    case _      => None
  }

  def replaceCell(updated: Cell) = {
    val index = updated.row * 3 + updated.col
    Board(cells.updated(index, updated))
  }

  override def toString = {
    val cols = (0 until 3)

    val rowsAndCols =
      cols.map(col => this(0, col)).mkString(",") + "\n" +
        cols.map(col => this(1, col)).mkString(",") + "\n" +
        cols.map(col => this(2, col)).mkString(",")

    rowsAndCols
  }
}

case class CardWithOwner(card: Card, owner: Player.Value) {
  override def toString = s"$owner$card"
}

case class Cell(card: Option[CardWithOwner], row: Int, col: Int, element: Element.Value = Element.None) {
  def placeCard(card: Card, owner: Player.Value) =
    copy(card = Some(CardWithOwner(card, owner)))

  def flip = {
    if (card.isDefined) {
      val currentOwner = card.get.owner
      val newOwner = if (currentOwner == Player.Red) Player.Blue else Player.Red

      copy(card = Some(card.get.copy(owner = newOwner)))
    } else {
      this
    }
  }

  def isEmpty = card.isEmpty

  def isOwnedBy(player: Player.Value) = {
    !isEmpty && card.get.owner == player
  }

  override def toString = card match {
    case Some(card) => card.toString
    case None       => "EMPTY{" + element + "}"
  }
}
