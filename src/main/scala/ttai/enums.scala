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
  val Lightning = Value("L")
  val Aero = Value("A")
  val None = Value("_")

  def forSymbol(symbol: Char) = {
    values.find(_.toString == symbol.toString)
      .getOrElse { throw new NoSuchElementException("No element found for symbol '" + symbol + "'") }
  }
}

object Direction extends Enumeration {
  val Left = Value("left")
  val Right = Value("right")
  val Bottom = Value("bottom2")
  val Top = Value("right")
}

