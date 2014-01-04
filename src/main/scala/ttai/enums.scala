package ttai

object Player extends Enumeration {
  val Blue = Value("B")
  val Red  = Value("R")

  def forSymbol(symbol: Char) = {
    values.find(_.toString == symbol.toString)
      .getOrElse { throw new NoSuchElementException("No player found for symbol '" + symbol + "'") }
  }
}

object Element extends Enumeration {
  val Fire = Value("F")
  val Thunder = Value("T")
  val Earth = Value("E")
  val Wind = Value("W")
  val Water = Value("O")
  val Poison = Value("P")
  val Ice = Value("I")
  val None = Value("_")

  def forSymbol(symbol: Char) = {
    values.find(_.toString == symbol.toString)
      .getOrElse { throw new NoSuchElementException("No element found for symbol '" + symbol + "'") }
  }
}

object Direction extends Enumeration {
  val Left = Value("left")
  val Right = Value("right")
  val Bottom = Value("bottom")
  val Top = Value("top")
}

object Rule extends Enumeration {
  val Same = Value("same")
  val Elemental = Value("elemental")
  val Plus = Value("plus")
}

