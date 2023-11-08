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
    val die1: Int = random.nextInt(5) + 1
    val die2: Int = random.nextInt(5) + 1
    movePlayer(name, die1, die2)                // rolled dice, ready to use
  }

  def movePlayer(name: String, die1: Int, die2: Int): Unit = {
    val movement: Int = die1 + die2
    val startingPosition: Int = players(name)
    val destinationPosition: Int = startingPosition + movement
    val movedPlayer = (name, destinationPosition)
    players = players + movedPlayer
    Printer.printOutMovement(name = name, die1 = die1, die2 = die2, start = startingPosition, end = destinationPosition)
    checkSpecialMovement(name, movement, destinationPosition)
  }

  def checkSpecialMovement(name: String, movement: Int, position: Int): Unit = {
    position match {
      case x if `gooseCell`.contains(x) => theGooseMove(name, movement, position)
      case `bridgeCell` => bridgeMove(name, movement, bridgeEndCell)
      case `endCell` => Printer.printOutMovementWin(name)
      case x if x>endCell => bounceMove(name, movement, position)
      case _ => println()
    }
  }

  def bounceMove(name: String, movement: Int, position: Int): Unit = {
    val bounced: Int = endCell - (position - endCell)
    val bouncedPlayer = (name, bounced)
    players = players + bouncedPlayer
    Printer.printOutMovementBounce(name, bounced)
    checkSpecialMovement(name, movement, bounced)
  }

  def bridgeMove(name: String, movement: Int, endCell: Int): Unit = {
    val jumpedPos = (name, endCell)
    players = players + jumpedPos
    Printer.printOutMovementBridge(name, endCell)
    checkSpecialMovement(name, movement, endCell)
  }

  def theGooseMove(name: String, movement: Int, position: Int): Unit = {
    val gooseDestination: Int = position + movement
    val goosedPlayer = (name, gooseDestination)
    players = players + goosedPlayer
    Printer.printOutMovementGoose(name, gooseDestination)
    checkSpecialMovement(name, movement, gooseDestination)
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

  def printOutMovement(name: String, die1: Int, die2: Int, start: Int, end: Int): Unit = {
    val p1: String = start match {
      case GooseGame.startCell => "Start"
      case GooseGame.bridgeCell => "The Bridge"
      case _ => start.toString
    }
    val p2: String = end match {
      case GooseGame.bridgeCell => "The Bridge"
      case x if x > GooseGame.endCell => GooseGame.endCell.toString
      case _ => end.toString
    }
    print(s"$name rolls $die1, $die2. $name moves from $p1 to $p2")
  }

  def printOutMovementWin(name: String): Unit = {
    println(s". $name Wins!!")
  }

  def printOutMovementBridge(name: String, destination: Int): Unit = {
    print(s". $name jumps to $destination")
  }

  def printOutMovementBounce(name: String, destination: Int): Unit = {
    print(s". $name bounces! $name returns to $destination")
  }

  def printOutMovementGoose(name: String, destination: Int): Unit = {
    print(s", The Goose. $name moves again and goes to $destination")
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
  GooseGame.movePlayerFull("Pippo", "2,", "2")
  GooseGame.movePlayerFull("Pippo", "1,", "1")
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
