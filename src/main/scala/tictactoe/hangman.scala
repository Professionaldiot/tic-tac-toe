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
TODO: use a list to update the words on screen
TODO: get user inputted text on screen to the swing Frame
TODO: from a list, take a random word from said list to get a random word
TODO: use swing to type user input in a box and check whether it's the correct word or not
TODO: get wrong letters written to swing Frame
TODO: add a way to update the man according to whether the letter was right or wrong
TODO: win detection
TODO: loss detection
 */

trait wordState :
  def word(userWord : String) : String

object word :
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

  def addCorrectGuess(): Unit = {
    var charToTake : Int = 0
    for char <- guess.newUserGuess do
      guess.guessBool(game.guessNew)
      if guess.wordBool then
        underscoreArray = underscoreArray.slice(0, charToTake+1)
        println(underscoreArray)
        underscoreArray += char
        guess.wordBool = false
      else
        var elem : Int = charToTake
        while elem <= userArray.length do
          if userArray(elem) == '\u0020' || userArray(elem) == '\u0009' || userArray(elem) == '\u000D' || userArray(elem) == '\u000A' then
            underscoreArray += ' '
          else if userArray(elem) == ',' then
            underscoreArray += ','
          else
            underscoreArray += '_'
          elem += 1
          guess.wordBool = false
      charToTake += 1

  }
  def toUnderscores(string : ArrayBuffer[Char]) : String = {
    for elem <- string do
      //addCorrectGuess()
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
  var newUserGuess = ""
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
class player extends wordState {
  var userWord = ""
  def currentWord() : Unit = {
    println(this.userWord)
  }
  override def word(userWord: String): String = {
    for char <- userWord do
      this.userWord += char
    println(this.userWord)
    this.userWord
  }
}

def newText() : Unit = {
  val user = new player()
  game.getNewWord
  game.nextRound
  game.newGuess()
  new Frame() {
    title = "HANGMAN"
    preferredSize = new Dimension(500,500)
    contents = new GridPanel(5,5){
      contents += new TextField("Type your guess in the console, use the button to start the check...", 25)
      contents += new TextField(f"${word.underscoredWord}")
      contents += new  ToggleButton("Check your guess!") {
        reactions += {
          case event.ButtonClicked(enabled_) =>

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
    val user = new player()
    val word = readLine()
    user.word(word)
    guess.correctWord = user.userWord

  }
  def newGuess() : String = {
    println("Type your guess below")
    val userGuess = readLine()
    guess.newUserGuess = userGuess
    guessNew = userGuess
    userGuess
  }
  def nextRound : Unit= {
    word.toArray(guess.correctWord)

    word.toUnderscores(word.userArray)

    word.copyArray()

  }
def hmMain(): Unit = {

  game.getNewWord

  println(word.underscoreArray)

  game.nextRound

  println(word.underscoreArray)

  println(word.underscoredWord)

  game.newGuess()

  println(word.underscoreArray)

  guess.guessBool(game.guessNew)

  println(word.underscoreArray)

  word.addCorrectGuess()

  println(word.underscoreArray)

}