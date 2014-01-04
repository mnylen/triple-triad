package ttai

import scala.util.Try

object Main extends App {
  def readCard(): Card = {
    def readRank(rank: Char) = {
      if (rank == 'A') {
        10
      } else {
        rank.toString.toInt
      }
    }

    val input = readLine("Enter card [TLRB{ELEMENT}]: ")
    Try {
      val rank = if (input.length > 4) {
        Element.forSymbol(input.charAt(5))
      } else {
        Element.None
      }

      Card(
        readRank(input.charAt(0)),
        readRank(input.charAt(1)),
        readRank(input.charAt(2)),
        readRank(input.charAt(3)),
        rank
      )
    }.getOrElse {
      println("Could not parse card from input.")
      readCard()
    }
  }

  def printMove(move: PlayCardTo) = {
    val position = move.row * 3 + move.col + 1
    println("Play card " + move.card + " to " + position)
  }

  val gameRules = Rule.values.flatMap(rule => {
    val answer = readLine("Does game have rule '" + rule + "'? ")
    if (answer == "yes") {
      Some(rule)
    } else {
      None
    }
  })

  println("Game rules are: " + gameRules.mkString(", "))
  println()

  val board = if (gameRules.contains(Rule.Elemental)) {
    val rows = readLine("Input cell elemental attributes: ")
      .split(" ")
      .map(e => Element.forSymbol(e.charAt(0)))
      .grouped(3)

    val cells = rows.zipWithIndex.map { case (cellElements, row) =>
      cellElements.zipWithIndex.map { case (element, col) =>
        Cell(None, row, col, element)
      }
    }.flatten.toList

    Board(cells)
  } else {
    Board.empty
  }

  println("Initial board state:")
  println(board.toString)

  println("Input hand for blue player:")
  val handForBlue = Hand((0 until 5).map(_ => readCard()).toList)

  println()
  println("Input hand for red player:")
  val handForRed = Hand((0 until 5).map(_ => readCard()).toList)

  println()
  val blueStarts = readLine("Does blue start? ")
  val whoStarts = if (blueStarts == "yes") Player.Blue else Player.Red

  var game = Game(board, whoStarts, handForBlue, handForRed, gameRules)
  while (!game.isOver) {
    if (game.whoseTurn == Player.Blue) {
      println("Searching best move for Blue player")
      val nextMove = Ai.getNextMove(game)
      if (nextMove.isDefined) {
        printMove(nextMove.get)
        game = game.playCard(nextMove.get.card, nextMove.get.row, nextMove.get.col)
      }
    } else {
      println("Move for red player:")
      val card = readCard()
      val position = readLine("position: ")

      game = Try {
        val row = (position.toInt - 1) / 3
        val col = position.toInt - (row * 3) - 1

        game.playCard(card, row, col)
      } getOrElse {
        println("Could not parse position")
        game
      }
    }

    println()
    println("Board is now:")
    println(game.board.toString)
    println()
  }
}
