import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random

object GooseGame {

  val startCell = 0
  val endCell = 63
  val bridgeCell = 6
  val bridgeEndCell = 12
  val gooseCell: List[Int] = List(5, 9, 14, 18, 23, 27)
  var players: Map[String, Int] = Map()

  def main(args: Array[String]): Unit = {
    Printer.printOutStart()
    commandLoop
  }

  def commandInput: String = {  //reading instruction method
    Printer.printOutInput()
    readLine()
  }

  @tailrec
  def commandLoop: Unit = {           //tail recursive game commands loop
    val inputCommand = commandInput   // read next command
    if (inputCommand=="exit") {       //exit will terminate the game
      Printer.printOutExit()
    }
    else {
      parseCommand(inputCommand)      // understand and do stuff
      commandLoop
    }
  }

  def parseCommand(command: String): Unit = {
    val splitCommand = command.split(" ").toList    // splits the string into words
    splitCommand match {
      case List("add", "player", name) => addPlayer(name)       // add player command
      case List("move", name, d1, d2) if players.contains(name) => movePlayerFull(name, d1, d2)   // move with numbers
      case List("move", name) if players.contains(name) => movePlayerHalf(name)    //move with random
      case _ => Printer.printOutInvalidCommandError()     // any invalid command
    }

  }

  def addPlayer(name: String): Unit = {
    if (players.contains(name))
      Printer.printOutDuplicatePlayer(name)    //if player exists (case sensitive) returns error message
    else {
      val newPlayer = (name, 0)
      players = players + newPlayer   // add player to player list
      Printer.printOutPlayers(players)
      }
  }

  def movePlayerFull(name: String, die1: String, die2: String): Unit = {    //move order received with numbers
    val check1 = die1.toCharArray
    val check2 = die2.toCharArray
    if ((check1(1) == ',') && (check1(0).isDigit) && (check1.size == 2) && (check2(0).isDigit) && (check2.size == 1)) {     //checking if the program received two single digit numbers (and the comma)
      val number1: Int = die1.take(1).toInt
      val number2: Int = die2.take(1).toInt      //only take the digit and turn it to Int
      if ((number1 <= 6) && (number2 <= 6)) {
        movePlayer(name, number1, number2) // string parsed and ready to use
      }
      else Printer.printOutCheaterError()
    }
    else Printer.printOutInvalidDieError()
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
    val start: String = p1 match {    // naming origin point special cases
      case `startCell` => "Start"
      case `bridgeCell` => "The Bridge"
      case _ => p1.toString
    }
    val end: String = p2 match {    // naming destination points special and bounce
      case `bridgeCell` => "The Bridge"
      case x if x>`endCell` => endCell.toString
      case _ => p2.toString
    }

    print(s" $name moves from $start to $end")
    val newPos = (name, p2)
    players = players + newPos    // movement done

    p2 match {              // check for goose, win, bounce or bridge
      case x if `gooseCell`.contains(x) => theGooseMove(name, movement, p2)    //Goose, this is a separate method in order to be called recursively
      case `bridgeCell` => {
        println(s". $name jumps to $bridgeEndCell")   //bridge
        val jumpedPos = (name, bridgeEndCell)
        players = players + jumpedPos
      }
      case `endCell` => println(s". $name Wins!!")  //Player wins
      case x if x>endCell => {
        val bounced: Int = endCell - (p2 - endCell)      //bounce
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
      case x if `gooseCell`.contains(x) => theGooseMove(name, movement, gooseP2)
      case _ => println()   //end of line for goose if none follow
    }
  }

}

object Printer {

  def printOutStart(): Unit = {
    println("Goose Game: started")
  }

  def printOutInput(): Unit = {
    print("Input command: ")
  }

  def printOutExit(): Unit = {
    println("Goose shutting down, have a nice day.")
  }

  def printOutInvalidCommandError(): Unit = {
    println("Command not recognized")
  }

  def printOutDuplicatePlayer(name: String): Unit = {
    println(s"$name: already existing player")
  }

  def printOutPlayers(playerList: Map[String, Int]): Unit = {
    println(s"players: ${playerList.keys.mkString(" ")}")
  }

  def printOutCheaterError(): Unit = {
    println("Hey! You cheat, you Boosted Shit!")
  }

  def printOutInvalidDieError(): Unit = {
    println("Dice roll is invalid")
  }

}

object Tester extends App {



  GooseGame.addPlayer("Pippo")
  GooseGame.addPlayer("Pippo")
  GooseGame.addPlayer("Pluto")
  GooseGame.movePlayerFull("Pippo", "4,", "6")
  GooseGame.movePlayerHalf("Pluto")
  GooseGame.movePlayerFull("Pippo", "12,", "6")
  GooseGame.movePlayerFull("Pippo", "4,", "6")
  GooseGame.movePlayerFull("Pippo", "4,", "6")
  GooseGame.movePlayerFull("Pippo", "4,", "6")
  GooseGame.movePlayerFull("Pippo", "4,", "6")
  GooseGame.movePlayerFull("Pippo", "4,", "6")
  GooseGame.movePlayerFull("Pippo", "4,", "6")
  GooseGame.movePlayerFull("Pippo", "2,", "3")
  GooseGame.movePlayerFull("Pippo", "2,", "1")
  GooseGame.addPlayer("Minnie")
  GooseGame.movePlayerFull("Minnie", "3,", "2")
  GooseGame.movePlayerFull("Minnie", "1,", "3")
  GooseGame.addPlayer("MikeyTheLuckyBastard")
  GooseGame.movePlayerFull("MikeyTheLuckyBastard", "3,", "3")
  GooseGame.movePlayerFull("MikeyTheLuckyBastard", "7,", "9")
  Printer.printOutPlayers(GooseGame.players)
  print("here I test the multiple Gooses:")
  GooseGame.theGooseMove("P1", 5, 18)



}
