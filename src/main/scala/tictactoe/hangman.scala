package tictactoe

import scala.swing.*
import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayBuffer.*
import scala.collection.mutable
import scala.collection.mutable.*
import scala.util.control.Breaks.*
/*
DONE: word state trait
TODO: get user inputted text on screen to the swing Frame
TODO: from a list, take a random word from said list to get a random word
SCRAPPED: use swing to type user input in a box and check whether it's the correct word or not
TODO: get wrong letters written to swing Frame
SCRAPPED: add a way to update the man according to whether the letter was right or wrong
DONE: win detection
DONE: loss detection
DONE: remove deprecated methods, traits and vals
DONE: using word.setLetters fill in the blank letters if they are right
 */

object word :
  var wrongLetters : ArrayBuffer[Any] = ArrayBuffer[Any]()
  var idx = 0
  private var wrongLetterFound = 0
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
      else if idx == underscoreArray.length-1 then
        if guess.correctWord(underscoreArray.length-1) == guess.newUserGuess then
          setLetters(underscoreArray, charToAdd, idx)
        else
          wrongLetters += guess.newUserGuess
      idx += 1
  }

  def toUnderscores(string : ArrayBuffer[Char]) : String = {
    for elem <- string do
      if elem == '\u0020' || elem == '\u0009' || elem == '\u000D' || elem == '\u000A' then
        underscoredWord += " > "
      else if elem == ',' then
        underscoredWord += ","
      else if elem == '\'' then
        underscoredWord += "\'"
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

object open :
  var orElse : Int = 0
def newText() : Unit = {
  if open.orElse >= 1 then
    run()
  open.orElse += 1
  if guess.correctWord == game.guessNew then
    println(f"you did it! you got ${guess.correctWord} in ${game.round} rounds!")
  else if game.round == 10 then
    println(f"you did not find the word ${guess.correctWord} in 10 rounds")
  else
    new Frame() {
      title = "HANGMAN"
      preferredSize = new Dimension(500, 500)
      contents = new GridPanel(5, 5) {
        contents += new TextField("Type your guess in the console, use the button to start the check...", 25)
        contents += new TextField(word.underscoreArray.toString().substring(12,word.underscoreArray.length*3 + 10))
        contents += new TextField("")
        contents += new ToggleButton("Make a guess...") {
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
      game.goNext
    }
}

object game :
  var round : Int = 0
  var guessNew = ""

  def goNext : Int = {
    round += 1
    round
  }
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
    guessNew += userGuess
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

  val printable = word.underscoreArray.toString().substring(12,word.underscoreArray.length*3 + 10)

  println(printable)

  println(word.wrongLetters)
}
def hmMain(): Unit = {

  game.getNewWord

  game.nextRound

  run()



  //newText()


}