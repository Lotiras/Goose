import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random

object GooseGame {

  var players: Map[String, Int] = Map()
  def main(args: Array[String]): Unit = {
    println("Goose Game: started")
    commandLoop

  }
  def commandInput: String = {  //reading instruction method
    print("Input command: ")
    readLine()
  }
  @tailrec
  def commandLoop: Unit = {           //tail recursive game commands loop
    val inputCommand = commandInput   // read next command
    if (inputCommand=="exit") {       //exit will terminate the game
      println("Goose shutting down, have a nice day.")
    }
    else {
      parseCommand(inputCommand)      // understand and do stuff
      commandLoop
    }
  }

  def parseCommand(command: String): Unit = {
    val splitCommand = command.split(" ").toList    // splits the string into words
    splitCommand match {
      case List("add", "player", name @ _) => addPlayer(name)       // add player command
      case List("move", name @_, d1@ _, d2@_) if players.contains(name) => movePlayerFull(name, d1, d2)   // move with numbers
      case List("move", name @_) if players.contains(name) => movePlayerHalf(name)    //move with random
      case _ => println("Command not recognized")     // any invalid command
    }

  }

  def addPlayer(name: String): Unit = {
    if (players.contains(name))
      println(s"$name: already existing player")    //if player exists (case sensitive) returns error message
    else {
      val newPlayer = (name, 0)
      players = players + newPlayer   // add player to player list
      print("players: ")
      printPlayers(players)     // print all players in list, inline
      println("")               // end of line
    }
  }

  @tailrec
  def printPlayers(p: Map[String, Int]): Unit = {   //print players in given list as strings
    if (p.nonEmpty) {
      print(p.head._1 + " ")
      printPlayers(p.tail)
    }
  }
  def movePlayerFull(name: String, d1: String, d2: String): Unit = {    //move order received with numbers
    val n1: Int= d1.take(1).toInt
    val n2: Int= d2.take(1).toInt
    if ((n1 <= 6) && (n2 <=6))
      movePlayer(name, n1, n2)      // string to int parsed and ready to use
    else
      println("Hey! You cheat, you Boosted Shit!")
  }
  def movePlayerHalf(name: String): Unit = {      // move order received w/o numbers
    val random = new Random(System.nanoTime())
    val n1: Int = random.nextInt(5) + 1
    val n2: Int = random.nextInt(5) + 1
    movePlayer(name, n1, n2)                // rolled dice, ready to use
  }
  def movePlayer(name: String, d1: Int, d2: Int): Unit = {    //prints first part inline, saves state, calls real move
    print(s"$name rolls $d1, $d2.")
    val movement: Int = d1 + d2
    val previousPosition: Int = players(name)
    val destinationPosition: Int = previousPosition + movement
    enactPlayerMovement(name, movement, previousPosition, destinationPosition)
  }

  def enactPlayerMovement(name: String, movement: Int, p1: Int, p2: Int): Unit ={   //actually moving now
    val start: String = p1 match {    // naming origin point specials
      case 0 => "Start"
      case 6 => "The Bridge"
      case _ => p1.toString
    }
    val end: String = p2 match {    // naming destination points special and bounce
      case 6 => "The Bridge"
      case x if x>63 => "63"
      case _ => p2.toString
    }
    print(s" $name moves from $start to $end")

    val newPos = (name, p2)
    players = players + newPos    // movement done

    p2 match { // check for goose, win, bounce or bridge
      case 5 | 9 | 14 | 18 | 23 | 27 => theGooseMove(name, movement, p2)    //Goose, this is a separate method in order to be called recursively
      case 6 => {           //bridge
        println(s". $name jumps to 12")
        val jumpedPos = (name, 12)
        players = players + jumpedPos
      }
      case 63 => {      //win
        println(s". $name Wins!!")

      }
      case x if x>63 => {     //bounce
        val bounced: Int = 63 - (p2 - 63)
        println(s". $name bounces! $name returns to $bounced")
        val bouncedPos = (name, bounced)
        players = players + bouncedPos
      }
      case _ => println()     //if none of the above, end of line
    }

  }

  @tailrec
  def theGooseMove(name: String, movement: Int, p2: Int): Unit = {    //move goose and print it
    val gooseP2: Int = p2 + movement
    val goosedPos = (name, gooseP2)
    players = players + goosedPos
    print(s", The Goose. $name moves again and goes to $gooseP2")
    gooseP2 match { // goose again?
      case 5 | 9 | 14 | 18 | 23 | 27 => theGooseMove(name, movement, gooseP2)
      case _ => println()   //end of line for goose if none follow
    }
  }

}

object Tester extends App {

  GooseGame.addPlayer("Pippo")
  GooseGame.addPlayer("Pippo")
  GooseGame.addPlayer("Pluto")
  GooseGame.movePlayerFull("Pippo", "4", "6")
  GooseGame.movePlayerHalf("Pluto")
  GooseGame.movePlayerFull("Pippo", "4", "6")
  GooseGame.movePlayerFull("Pippo", "4", "6")
  GooseGame.movePlayerFull("Pippo", "4", "6")
  GooseGame.movePlayerFull("Pippo", "4", "6")
  GooseGame.movePlayerFull("Pippo", "4", "6")
  GooseGame.movePlayerFull("Pippo", "4", "6")
  GooseGame.movePlayerFull("Pippo", "2", "3")
  GooseGame.movePlayerFull("Pippo", "2", "1")
  GooseGame.addPlayer("Minnie")
  GooseGame.movePlayerFull("Minnie", "3", "2")
  GooseGame.movePlayerFull("Minnie", "1", "3")
  GooseGame.addPlayer("MikeyTheLuckyBastard")
  GooseGame.movePlayerFull("MikeyTheLuckyBastard", "3", "3")
  GooseGame.movePlayerFull("MikeyTheLuckyBastard", "7", "9")
  print("here I test the multiple Gooses:")
  GooseGame.theGooseMove("P1", 5, 18)

}
