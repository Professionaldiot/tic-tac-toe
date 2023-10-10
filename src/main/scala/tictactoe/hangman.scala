package tictactoe

import scala.swing.*
import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayBuffer.*
import scala.collection.mutable
import scala.collection.mutable.*
/*
TODO: maybe use a trait to do word state?
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
  def setWord(userWord : String) : String = word(userWord)
object word :
  var userArray : ArrayBuffer[Char] = ArrayBuffer[Char]() //including spaces
  def toArray(word : String) : Any = {
    for i <- word do
      userArray += i
  }
class player extends wordState {
  private var userWord = ""
  def currentWord() : Unit = {
    println(this.userWord)
  }
  override def word(userWord: String): String = {
    for char <- userWord do 
      this.userWord += char
    println("" + setWord(this.userWord))
    setWord(this.userWord)
  }
}

def hmMain(): Unit = {
  val user = new player()
  user.word("test")
  println()
}