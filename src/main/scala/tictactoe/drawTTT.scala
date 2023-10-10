package tictactoe

import cats.implicits.*
import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.java2d.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.image.syntax.core.*
import doodle.reactor.*
//import doodle.svg.*
import doodle.syntax.angle

def notMain() : Unit = {
  val blackSquare = Image.rectangle(90, 90).fillColor(Color.black)
  val redSquare = Image.rectangle(90, 90).fillColor(Color.red)
  val whiteSquare = Image.rectangle(90, 90).fillColor(Color.white)

  // A chessboard, broken into steps showing the recursive construction
  val oneLine =
    redSquare.above(redSquare).above(redSquare)

  val twoLine =
    blackSquare.above(blackSquare).above(blackSquare)

  val chessboard =
    oneLine.beside(twoLine).beside(oneLine)
  
  chessboard.draw()
}