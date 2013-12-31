package ttai

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

case class Cell(card: Option[CardWithOwner], row: Int, col: Int) {
  // TODO: cell might have an element too
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

  override def toString = card match {
    case Some(card) => card.toString
    case None       => "EMPTY{_}"
  }
}
