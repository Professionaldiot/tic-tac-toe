package tictactoe

import scala.swing.*
import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayBuffer.*
import scala.collection.mutable
import scala.collection.mutable.*
import scala.language.postfixOps
import scala.swing.event.KeyPressed.*
import scala.swing.event.Key.*
import scala.swing.event.Key.Location
import scala.util.control.Breaks.*
/*
DONE: word state trait
TODO: get user inputted text on screen to the swing Frame
TODO: from a list, take a random word from said list to get a random word
SCRAPPED: use swing to type user input in a box and check whether it's the correct word or not
TODO: get wrong letters written to swing Frame
TODO: add a way to update the man according to whether the letter was right or wrong
TODO: win detection
TODO: loss detection
DONE: remove deprecated methods, traits and vals
DONE: using word.setLetters fill in the blank letters if they are right
 */

object word :
  var idx = 0
  var underscoredWord : String = ""
  val userArray : ArrayBuffer[Char] = ArrayBuffer[Char]() //including spaces
  var underscoreArray : ArrayBuffer[Char] = ArrayBuffer[Char]()
  def copyArray() : Unit = {
    for elem <- underscoredWord do if elem != ' ' then underscoreArray += elem
  }
  def toArray(word : String) : Any = {
    for i <- word do
      userArray += i
  }

  def setLetters(userArray : ArrayBuffer[Char], charsToAdd : String, indexToAddCharsAt: Int): ArrayBuffer[Char] = {
    val lettersArray = charsToAdd.toList
    lettersArray.zipWithIndex.foreach {
      case(correctLetters, arrayIndex) => userArray(arrayIndex + indexToAddCharsAt) = correctLetters
    }
    userArray
  }

  def addLetters() : Unit = {
    idx = 0
    while idx < underscoreArray.length do
      val charToAdd = guess.newUserGuess.toString
      if guess.correctWord(idx) == guess.newUserGuess then
        setLetters(underscoreArray, charToAdd, idx)
      idx += 1
  }

  def toUnderscores(string : ArrayBuffer[Char]) : String = {
    for elem <- string do
      if elem == '\u0020' || elem == '\u0009' || elem == '\u000D' || elem == '\u000A' then
        underscoredWord += " > "
      else if elem == ',' then
        underscoredWord += ","
      else
        underscoredWord += "_ "
    underscoredWord
  }

object guess :
  var correctWord = ""
  var newUserGuess = ' '
  var wordBool = false
  def guessBool(guess : String) : Unit = {
    if guess == correctWord then wordBool = true
    else if guess.length == 1 then
      breakable {
        for elem <- correctWord do
          if elem == guess(0) then
            wordBool = true
            break
          else wordBool = false
      }
    else wordBool = false
  }

object oneTutu :
  var oneTutu : Int = 0
def newText() : Unit = {
  if oneTutu.oneTutu >= 1 then
    run()
  new Frame() {
    title = "HANGMAN"
    preferredSize = new Dimension(500, 500)
    contents = new GridPanel(5, 5) {
      contents += new TextField("Type your guess in the console, use the button to start the check...", 25)
      contents += new TextField("" + word.underscoreArray)
      contents += new ToggleButton("Make a guess...") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            close()
            newTextTwo()
        }
      }
    }
    pack()
    centerOnScreen()
    open()
  }
}

def newTextTwo() : Unit = {
  oneTutu.oneTutu += 1
  run()
  new Frame() {
    title = "HANGMAN"
    preferredSize = new Dimension(500,500)
    contents = new GridPanel(5,5){
      contents += new TextField("Type your guess in the console, use the button to start the check...", 25)
      contents += new TextField("" + word.underscoreArray)
      contents += new  ToggleButton("Make a guess...") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            close()
            newText()
        }
      }

    }
    pack()
    centerOnScreen()
    open()
  }
}

object game :
  var guessNew = ""
  def getNewWord : Any = {
    println("Type your word in the console below this.")
    val word = readLine()
    guess.correctWord = word

  }
  def newGuess() : Unit = {
    println("Type your guess below")
    val userGuess = readLine()
    var userChar : Char = ' '
    for elem <- userGuess do userChar = elem
    guess.newUserGuess = userChar
    guessNew = userGuess
  }
  def nextRound : Unit= {

    word.toArray(guess.correctWord)

    word.toUnderscores(word.userArray)

    word.copyArray()

  }

def run() : Unit = {

  game.newGuess()

  guess.guessBool(game.guessNew)

  word.addLetters()

  //println(guess.newUserGuess)

  println(word.underscoreArray)
}
def hmMain(): Unit = {

  game.getNewWord

  game.nextRound

  newText()

}