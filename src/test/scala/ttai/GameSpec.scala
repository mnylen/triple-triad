package ttai

import org.scalatest.{ShouldMatchers, FunSpec}

class GameSpec extends FunSpec with ShouldMatchers {
  describe("a game") {
    it("should start with empty board") {
      val game = Game(Player.Blue, Hand.empty, Hand.empty)

      for {
        row <- (0 until 3)
        col <- (0 until 3)
      } game.board(row, col) should be(Cell(None, row, col))
    }

    it("should play the first card with the given player, and then give turn to opposing player") {
      val card = Card(3, 4, 2, 1)
      val game = Game(Player.Blue, Hand.empty, Hand.empty)

      val updated = game.playCard(card, 0, 0)
      updated.whoseTurn should be(Player.Red)
      updated.board.toString should be(
        """
          |B3421{_},EMPTY{_},EMPTY{_}
          |EMPTY{_},EMPTY{_},EMPTY{_}
          |EMPTY{_},EMPTY{_},EMPTY{_}
        """.stripMargin.trim)
    }

    it("should play second card with the opposing player and then give turn to the player who started") {
      val card = Card(3, 4, 2, 1)
      val game = Game(Player.Blue, Hand.empty, Hand.empty)

      val updated = game
        .playCard(card, 0, 0) // blue turn
        .playCard(card, 2, 2) // red turn

      updated.whoseTurn should be(Player.Blue)
      updated.board.toString should be(
        """
          |B3421{_},EMPTY{_},EMPTY{_}
          |EMPTY{_},EMPTY{_},EMPTY{_}
          |EMPTY{_},EMPTY{_},R3421{_}
        """.stripMargin.trim)
    }
  }

  describe("basic rules") {
    describe("flipping by placing card below another card") {
      val game = GameUtil.parseGame(
        """
          |R4321{_},EMPTY{_},EMPTY{_}
          |EMPTY{_},EMPTY{_},EMPTY{_}
          |EMPTY{_},EMPTY{_},EMPTY{_}
        """.stripMargin.trim)

      it("should flip the card above if the placed card has higher rank on touching side") {
        val B2342 = Card(2,3,4,2)

        val updated = game.playCard(B2342, 1, 0)
        updated.board.toString should be(
          """
            |B4321{_},EMPTY{_},EMPTY{_}
            |B2342{_},EMPTY{_},EMPTY{_}
            |EMPTY{_},EMPTY{_},EMPTY{_}
          """.stripMargin.trim)
      }

      it("should not flip the card if the placed card does not have higher rank on touching side") {
        val B1234 = Card(1,2,3,4)

        val updated = game.playCard(B1234, 1, 0)
        updated.board.toString should be(
          """
            |R4321{_},EMPTY{_},EMPTY{_}
            |B1234{_},EMPTY{_},EMPTY{_}
            |EMPTY{_},EMPTY{_},EMPTY{_}
          """.stripMargin.trim)
      }
    }

    describe("flipping by placing card above another card") {
      val game = GameUtil.parseGame(
        """
          |EMPTY{_},EMPTY{_},EMPTY{_}
          |EMPTY{_},EMPTY{_},EMPTY{_}
          |EMPTY{_},R4321{_},EMPTY{_}
        """.stripMargin.trim)

      it("should flip the card below if the placed card has higher rank on touching side") {
        val B1115 = Card(1,1,1,5)

        val updated = game.playCard(B1115, 1, 1)
        updated.board.toString should be(
          """
            |EMPTY{_},EMPTY{_},EMPTY{_}
            |EMPTY{_},B1115{_},EMPTY{_}
            |EMPTY{_},B4321{_},EMPTY{_}
          """.stripMargin.trim)
      }

      it("should not flip the card if the placed card does not have higher rank on touching side") {
        val B1234 = Card(1,2,3,4)

        val updated = game.playCard(B1234, 1, 1)
        updated.board.toString should be(
          """
            |EMPTY{_},EMPTY{_},EMPTY{_}
            |EMPTY{_},B1234{_},EMPTY{_}
            |EMPTY{_},R4321{_},EMPTY{_}
          """.stripMargin.trim)
      }
    }

    describe("flipping by placing card to right of another card") {
      val game = GameUtil.parseGame(
        """
          |EMPTY{_},EMPTY{_},EMPTY{_}
          |EMPTY{_},R4321{_},EMPTY{_}
          |EMPTY{_},EMPTY{_},EMPTY{_}
        """.stripMargin.trim)

      it("should flip the card on left if the placed card has higher rank on touching side") {
        val B1311 = Card(1,3,1,1)

        val updated = game.playCard(B1311, 1, 2)
        updated.board.toString should be(
          """
            |EMPTY{_},EMPTY{_},EMPTY{_}
            |EMPTY{_},B4321{_},B1311{_}
            |EMPTY{_},EMPTY{_},EMPTY{_}
          """.stripMargin.trim)
      }

      it("should not flip the card on left if the placed card has lower or same rank on touching side") {
        val B1111 = Card(1,1,1,1)

        val updated = game.playCard(B1111, 1, 2)
        updated.board.toString should be(
          """
            |EMPTY{_},EMPTY{_},EMPTY{_}
            |EMPTY{_},R4321{_},B1111{_}
            |EMPTY{_},EMPTY{_},EMPTY{_}
          """.stripMargin.trim)
      }
    }

    describe("flipping by placing card to left of another card") {
      val game = GameUtil.parseGame(
        """
          |EMPTY{_},EMPTY{_},EMPTY{_}
          |EMPTY{_},R4321{_},EMPTY{_}
          |EMPTY{_},EMPTY{_},EMPTY{_}
        """.stripMargin.trim)

      it("should flip the card on right if the placed card has higher rank on touching side") {
        val B1141 = Card(1,1,4,1)

        val updated = game.playCard(B1141, 1, 0)
        updated.board.toString should be(
          """
            |EMPTY{_},EMPTY{_},EMPTY{_}
            |B1141{_},B4321{_},EMPTY{_}
            |EMPTY{_},EMPTY{_},EMPTY{_}
          """.stripMargin.trim)
      }

      it("should not flip the card on right if the placed card has lower or same rank on touching side") {
        val B1131 = Card(1,1,3,1)

        val updated = game.playCard(B1131, 1, 0)
        updated.board.toString should be(
          """
            |EMPTY{_},EMPTY{_},EMPTY{_}
            |B1131{_},R4321{_},EMPTY{_}
            |EMPTY{_},EMPTY{_},EMPTY{_}
          """.stripMargin.trim)
      }
    }

    describe("placing cards next to own cards") {
      it("should not flip own cards") {
        val B5322 = Card(5,3,2,2)

        val game = GameUtil.parseGame(
          """
            |EMPTY{_},B1234{_},EMPTY{_}
            |R4321{_},EMPTY{_},EMPTY{_}
            |EMPTY{_},EMPTY{_},EMPTY{_}
          """.stripMargin.trim)

        val updated = game.playCard(B5322 ,1, 1)
        updated.board.toString should be(
          """
            |EMPTY{_},B1234{_},EMPTY{_}
            |B4321{_},B5322{_},EMPTY{_}
            |EMPTY{_},EMPTY{_},EMPTY{_}
          """.stripMargin.trim)
      }
    }

    describe("flipping multiple cards at once") {
      val game = GameUtil.parseGame(
        """
          |EMPTY{_},R1234{_},EMPTY{_}
          |R1234{_},EMPTY{_},R1234{_}
          |EMPTY{_},R1234{_},EMPTY{_}
        """.stripMargin.trim)

      it("should flip card on right and below with one card") {
        game.playCard(Card(1,1,3,2), 0, 0).board.toString should be(
          """
            |B1132{_},B1234{_},EMPTY{_}
            |B1234{_},EMPTY{_},R1234{_}
            |EMPTY{_},R1234{_},EMPTY{_}
          """.stripMargin.trim)
      }

      it("should flip card on left and below with one card") {
        game.playCard(Card(1,4,1,2), 0, 2).board.toString should be(
          """
            |EMPTY{_},B1234{_},B1412{_}
            |R1234{_},EMPTY{_},B1234{_}
            |EMPTY{_},R1234{_},EMPTY{_}
          """.stripMargin.trim)
      }

      it("should flip card above, below, left and right with one card") {
        game.playCard(Card(5,5,5,5), 1, 1).board.toString should be(
          """
            |EMPTY{_},B1234{_},EMPTY{_}
            |B1234{_},B5555{_},B1234{_}
            |EMPTY{_},B1234{_},EMPTY{_}
          """.stripMargin.trim)
      }

      it("should flip card right & above with one card") {
        game.playCard(Card(5,4,3,2), 2, 0).board.toString should be(
          """
            |EMPTY{_},R1234{_},EMPTY{_}
            |B1234{_},EMPTY{_},R1234{_}
            |B5432{_},B1234{_},EMPTY{_}
          """.stripMargin.trim)
      }

      it("should flip card left & above with one card") {
        game.playCard(Card(5,4,3,2), 2, 2).board.toString should be(
          """
            |EMPTY{_},R1234{_},EMPTY{_}
            |R1234{_},EMPTY{_},B1234{_}
            |EMPTY{_},B1234{_},B5432{_}
          """.stripMargin.trim)
      }
    }
  }
}
