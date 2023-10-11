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
  var userArray : ArrayBuffer[Char] = ArrayBuffer[Char]() //including spaces
  def toArray(word : String) : Any = {
    for i <- word do
      userArray += i
  }
  def toUnderscores(string : ArrayBuffer[Char]) : String = {
    for elem <- string do
      if elem == '\u0020' || elem == '\u0009' || elem == '\u000D' || elem == '\u000A' then
        underscoredWord += " >  "
      else
        underscoredWord += "_ "
      if elem == ',' then
        underscoredWord += ","
    underscoredWord
  }

object guess :
  var correctWord = ""
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

def newText(userGuess : String) : Unit = {
  val user = new player()
  new Frame() {
    title = "HANGMAN"
    preferredSize = new Dimension(500,500)

    contents = new GridPanel(5,5){
      contents += new TextField("Type your guess in the console, use the button to start the check...", 25)
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

def getNewWord : Any = {
  println("Type your word in the console below this.")
  val user = new player()
  val word = readLine()
  user.word(word)
  guess.correctWord = user.userWord

}

def hmMain(): Unit = {
  /*
  val user = new player()
  println("i am here")
  user.word("test")
  println(user.currentWord())
  */
  getNewWord
  word.toArray(guess.correctWord)
  word.toUnderscores(word.userArray)
  println(word.userArray)
  println()
  println(guess.correctWord)
  println(word.underscoredWord)
}