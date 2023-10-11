package tictactoe


import scala.swing.*
import scala.io.StdIn.readLine

/*
DONE: win detection
SCRAPPED: get swing interacting with doodle using the contents +=
DONE: add tie detection
DONE: add states for buttons (meaning once you press one you can't press it again)
DONE: add a quick reset button
DONE: back and forth game play
DONE: position selection using swing?

using matrices' to hold positions (arrays of arrays of ints)
numbers represent our player(1 and 2)

 */

trait boardState :
  def currentState(board : Array[Array[Int]]) : Unit

object board :
  var bard : Array[Array[Int]] = Array.ofDim[Int](3,3)

class boardAdder extends boardState {
  def bored(player : Int, x : Int, y : Int): Unit = {
    board.bard(x)(y) = player
  }

  override def currentState(args: Array[Array[Int]]): Unit = {
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        print(args(i)(j) + " ")
      }
      println()
    }
  }
}

def newButton() : Unit = {
  val player = new boardAdder()
  new Frame() {
    title = "TIC-TAC-TOE"
    preferredSize = new Dimension(500,500)
    contents = new GridPanel(5,5) {
      contents += new Label("Tic")
      contents += new Label("Tac")
      contents += new Label("Toe")

      contents += new ToggleButton("1") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            player.bored(round.player, 0, 0)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("4") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            player.bored(round.player, 0,1)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("7") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            player.bored(round.player, 0,2)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("2") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            player.bored(round.player, 1, 0)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("5") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            player.bored(round.player, 1, 1)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("8") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            player.bored(round.player, 1,2)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("3") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            player.bored(round.player, 2, 0)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("6") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            player.bored(round.player, 2,1)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("9") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            player.bored(round.player, 2, 2)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("Reset") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            println("Resetting...")
            for (i <- 0 until 3) {
              for (j <- 0 until 3) {
                board.bard(i)(j) = 0
              }
            }
            round.player = 1
            round.tieCounter = 0
            round.roundNum = 0
            close()
            main()
        }
      }
    }
    pack()
    centerOnScreen()
    open()

  }
}
object win :
  def bundle = {
    round.nextRound
    winner.horiz(board.bard)
    winner.vertical(board.bard)
    winner.diagonal(board.bard)
    if round.tieChecker == true then println("There is a tie, no one won.")
  }

object winner :
  private var i = 0
  def horiz(board : Array[Array[Int]]): Unit = {
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        this.i += 1
        if this.i == 3 then
          this.i = 0
          if board(i)(j) == 0 then
            print(" ")
          else if board(i)(j) == board(i)(j-2) && board(i)(j) == board(i)(j-1) then
            println()
            round.nextRound
            println("player " + round.player + " has won")
          if round.roundNum == 9 then
            if !(board(i)(j) == board(i)(j-2) && board(i)(j) == board(i)(j-1)) then
              round.tieCounter += 1
      }
    }
  }

  def vertical(board: Array[Array[Int]]): Unit = {
    for (i <- 0 until 1) {
      for (j <- 0 until 3) {
        if board(i)(j) == 0 then
          print(" ")
        else if board(i)(j) == board(i+2)(j) && board(i)(j) == board(i+1)(j) then
          println()
          round.nextRound
          println(board(i)(j) + " ")
          println("player " + round.player + " has won")
        if round.roundNum == 9 then
          if !(board(i)(j) == board(i + 2)(j) && board(i)(j) == board(i + 1)(j)) then
            round.tieCounter += 1
      }
    }
  }
  def diagonal(board : Array[Array[Int]]): Unit = {
    if board(0)(0) == board(1)(1) && board(0)(0) == board(2)(2) then
      if board(0)(0) == 0 then
        print(" ")
      else
        println()
        round.nextRound
        println("player " + round.player + " has won")
    else if board(0)(2) == board(1)(1) && board(0)(2) == board(2)(0) then
      if board(0)(2) == 0 then
        print(" ")
      else
        println()
        round.nextRound
        println("player " + round.player + " has won")
    if round.roundNum == 9 then
      if !(board(0)(0) == board(1)(1) && board(0)(0) == board(2)(2)) || !(board(0)(2) == board(1)(1) && board(0)(2) == board(2)(0)) then
        round.tieCounter += 1
  }

object round :
  var player : Int = 1
  var roundNum : Int = 0
  var tieCounter : Int = 0
  def nextRound = {
    if player == 1 then
      roundNum += 1
      player = 2
    else if player == 2 then
      roundNum += 1
      player = 1
    else
      player = 1
  }
  def check(player : Int) : Int = {
    this.player
  }
  def tieChecker  : Boolean = {
    if roundNum == 9 then
      if tieCounter >= 0 then true
      else false
    else false
  }

def main() : Unit = {
  newButton()
}
@main def tttMain() : Unit = {
  /*
  println("Press 1 for Tic-Tac-Toe or press 2 for Hangman")
  val user = readLine()
  if user == "1" then
    newButton()
  else
    hmMain()
    println()

   */
  hmMain()

}