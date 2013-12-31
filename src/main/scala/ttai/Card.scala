package ttai

case class Card(rankTop: Int, rankLeft: Int, rankRight: Int, rankBottom: Int, element: Element.Value = Element.None) {
  import Direction._

  def rank(side: Direction.Value) = side match {
    case Left   => rankLeft
    case Right  => rankRight
    case Top    => rankTop
    case Bottom => rankBottom
  }

  def oppositeRank(side: Direction.Value) = side match {
    case Left   => rankRight
    case Right  => rankLeft
    case Top    => rankBottom
    case Bottom => rankTop
  }

  override def toString = {
    s"$rankTop$rankLeft$rankRight$rankBottom{$element}"
  }
}


case class Hand(cards: List[Card]) {
  def withoutCard(card: Card) = {
    Hand(cards.filter(_ != card))
  }
}

object Hand {
  val empty = Hand(List.empty)
}

