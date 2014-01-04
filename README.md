# Triple Triad AI

A triple triad AI based on negamax algorithm. Currently supports the following rules:

* Plus (with combo)
* Elemental
* Open

Current implementation won Ellone seven times out of eight, with one game ending in Draw with Sudden Death. :)

## Usage

Execute

    sbt run

Follow the instructions, and read the source in `Main.scala` first.

Beware of typos - there's no undo!

## TODO

### Refactoring

* More tests for Plus rule (especially when elemental rule is in effect)
* Tests for elemental rule
* Separate game logic & game state
* Remove CardWithOwner and let the Card know who owns it
* Add holy elemental attribute

### Features

* Web UI
* Support for Same/Same-Wall rule
* Tests for elemental rule
* Support for "Closed" rule (meaning, game is not played with "Open" rule)
* Support for Sudden Death rule

#### "Closed" rule:

* on every turn, pick 5, 4, 3 or 2 cards randomly for red player
* try to play against the random hand
