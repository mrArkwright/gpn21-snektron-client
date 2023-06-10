import java.io.*
import java.net.Socket
import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@main
def main(): Unit = {
  val serverAddress = sys.env("SERVER_ADDRESS")
  val port = sys.env("SERVER_PORT").toInt

  val username = sys.env("USERNAME")
  val password = sys.env("PASSWORD")

  val socket = Socket(serverAddress, port)

  val readLoopFuture = Future(readLoop(socket, username, password))

  Await.result(readLoopFuture, Duration.Inf)

  socket.close()
}

def readLoop(socket: Socket, username: String, password: String): Unit = {
  val inputStream = new DataInputStream(socket.getInputStream)
  val outputStream = new DataOutputStream(socket.getOutputStream)

  //val bufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream))

  sendMessage(ClientMessage.Join(username, password), outputStream)

  var message = ""
  var gameState = GameState.Idle

  while ({
    message = inputStream.readLine(); message != null
  }) {
    val currentTimestamp = System.currentTimeMillis()
    val timeDifference = currentTimestamp - lastTickTimestamp

    val parsedMessage = parseServerMessage(message)
    println(s"Received: $parsedMessage, raw: $message, timeDifference: $timeDifference")
    if (parsedMessage == ServerMessage.Tick) {
      lastTickTimestamp = currentTimestamp
    }

    gameState = handleServerMessage(gameState, parsedMessage, outputStream)
  }
}

def handleServerMessage(gameState: GameState, message: ServerMessage, outputStream: DataOutputStream): GameState =
  message match
    case ServerMessage.Motd(message) =>
      gameState

    case ServerMessage.Error(message) =>
      gameState

    case ServerMessage.Game(width, height, playerId) =>
      sendRandomChatMessage(outputStream)
      GameState.Starting(width, height, playerId)

    case ServerMessage.Pos(playerId, x, y) =>
      val gameState1 = gameState match
        case GameState.Starting(width, height, myPlayerId) if playerId == myPlayerId =>
          println("*******************************")
          val gameBoard = GameBoard(width, height)
          val position = Position(x, y)
          GameState.Running(playerId, position, gameBoard)
        case gameState @ GameState.Running(myPlayerId, _, _, _, _) if playerId == myPlayerId =>
          gameState.copy(position = Position(x, y))
        case _ =>
          gameState

      gameState1.mapRunning { running =>
        val newGameBoard = running.gameBoard.updatePlayerPosition(playerId, x, y)
        val newPlayerPositions = running.playerPositions + (playerId -> Position(x, y))
        running.copy(gameBoard = newGameBoard, playerPositions = newPlayerPositions)
      }

    case ServerMessage.Tick =>
      gameState.mapRunning { running =>
        println("-------------------")
        println(running.print)

        val nextDirection = computeNextDirection(running)

        println(s"playerId: ${running.playerId}")
        println(s"position: ${running.position}")
        //println(s"freeDirections: $freeDirections")
        println(s"nextDirection: $nextDirection")
        println("-------------------")

        sendMessage(ClientMessage.Move(nextDirection), outputStream)

        running.copy(lastDirection = Some(nextDirection))
      }

    case ServerMessage.Die(playerIds) =>
      gameState.mapRunning { running =>
        val newGameBoard = running.gameBoard.removePlayers(playerIds)
        running.copy(gameBoard = newGameBoard)
      }

    case ServerMessage.Message(playerId, message) =>
      gameState

    case ServerMessage.Win(amountWins, amountLoses) =>
      println("\uD83C\uDFC6 You won! ")
      GameState.Idle

    case ServerMessage.Loose(amountWins, amountLoses) =>
      println("☠\uFE0F You lost!")
      GameState.Idle

def computeNextDirection(runningGameState: GameState.Running): Direction = {
  val freeDirections = getFreeDirections(runningGameState.gameBoard, runningGameState.position)

  val adjacentPositions = getAdjacentPositions(runningGameState.position, runningGameState.gameBoard)

  val positionsAdjacentToOtherPlayers = runningGameState.playerPositions
    .filter { case (playerId, position) => playerId != runningGameState.playerId }
    .flatMap { case (playerId, position) => getAdjacentPositions(position, runningGameState.gameBoard) }
    .map { case (direction, position) => position }
    .toSet

  val directionsAdjacentToOtherPlayers = adjacentPositions
    .filter { case (_, position) => positionsAdjacentToOtherPlayers.contains(position) }
    .map { case (direction, _) => direction }

  val choicesLevel3 = freeDirections
  val choicesLevel2 = choicesLevel3.filterNot(directionsAdjacentToOtherPlayers.contains)
  val choiceLevel1 = runningGameState.lastDirection.filter(choicesLevel2.contains)
  val choiceLevel0 = runningGameState.lastDirection.map(rightTurn).filter(choicesLevel2.contains)

  val nextDirection = choiceLevel0
    .orElse(choiceLevel1)
    .orElse(choicesLevel2.headOption)
    .orElse(choicesLevel3.headOption)
    .getOrElse(Direction.Up)

  //val nextDirection = freeDirections.headOption.getOrElse(Direction.Down)

  //val nextDirection =
  //  if (freeDirections.isEmpty) Direction.Down
  //  else freeDirections(scala.util.Random.nextInt(freeDirections.length))

  nextDirection
}

def rightTurn(direction: Direction): Direction =
  direction match
    case Direction.Up => Direction.Right
    case Direction.Right => Direction.Down
    case Direction.Down => Direction.Left
    case Direction.Left => Direction.Up

def getFreeDirections(gameBoard: GameBoard, position: Position): List[Direction] =
  getAdjacentPositions(position, gameBoard)
    .filter { case (_, position) => gameBoard.playerAt(position).isEmpty }
    .map { case (direction, _) => direction }

def getAdjacentPositions(position: Position, gameBoard: GameBoard): List[(Direction, Position)] =
  List(
    (Direction.Down, position.addOffset(0, 1, gameBoard.width, gameBoard.height)),
    (Direction.Left, position.addOffset(-1, 0, gameBoard.width, gameBoard.height)),
    (Direction.Up, position.addOffset(0, -1, gameBoard.width, gameBoard.height)),
    (Direction.Right, position.addOffset(1, 0, gameBoard.width, gameBoard.height))
  )


def parseServerMessage(unparsedMessage: String): ServerMessage = {
  val messageFields = unparsedMessage.split('|').toList
  messageFields match {
    case List("motd", messageOfTheDay) => ServerMessage.Motd(messageOfTheDay)
    case List("error", errorMessage) => ServerMessage.Error(errorMessage)
    case List("game", gameWidth, gameHeight, playerId) => ServerMessage.Game(gameWidth.toInt, gameHeight.toInt, playerId)
    case List("pos", playerId, x, y) => ServerMessage.Pos(playerId, x.toInt, y.toInt)
    case List("tick") => ServerMessage.Tick
    case "die" :: playerIds => ServerMessage.Die(playerIds)
    case List("message", playerId, message) => ServerMessage.Message(playerId, message)
    case List("win", amountWins, amountLoses) => ServerMessage.Win(amountWins.toInt, amountLoses.toInt)
    case List("lose", amountWins, amountLoses) => ServerMessage.Loose(amountWins.toInt, amountLoses.toInt)
  }
}

def sendRandomChatMessage(outputStream: DataOutputStream): Unit = {
  val randomMessage = chatMessages(scala.util.Random.nextInt(chatMessages.length))
  sendMessage(ClientMessage.Chat(randomMessage), outputStream)
}

def sendMessage(message: ClientMessage, outputStream: DataOutputStream): Unit = {
  val messageString = message.toProtocolMessage
  outputStream.writeBytes(messageString + "\n")
  outputStream.flush()

  val currentTimestamp = System.currentTimeMillis()
  val timeDifference = currentTimestamp - lastTickTimestamp

  println(s"Sent: $messageString, timeDifference: $timeDifference")
}

enum ServerMessage:
  case Motd(message: String)
  case Error(message: String)
  case Game(width: Int, height: Int, playerId: String)
  case Pos(playerId: String, x: Int, y: Int)
  case Tick
  case Die(playerIds: List[String])
  case Message(playerId: String, message: String)
  case Win(amountWins: Int, amountLoses: Int)
  case Loose(amountWins: Int, amountLoses: Int)

enum ClientMessage:
  case Join(username: String, password: String)
  case Move(direction: Direction)
  case Chat(message: String)

  def toProtocolMessage: String = this match
    case Join(username, password) => s"join|$username|$password"
    case Move(direction) => s"move|${direction.toString.toLowerCase}"
    case Chat(message) => s"chat|$message"

enum Direction:
  case Up
  case Down
  case Left
  case Right

enum GameState:
  case Idle
  case Starting(width: Int, height: Int, playerId: String)
  case Running(
    playerId: String,
    position: Position,
    gameBoard: GameBoard,
    playerPositions: Map[String, Position] = Map.empty,
    lastDirection: Option[Direction] = None
  )

  def mapRunning(f: Running => Running): GameState = this match
    case Running(playerId, position, gameBoard, lastDirection, playerPositions) => f(Running(playerId, position, gameBoard, lastDirection, playerPositions))
    case _ => this

extension (running: GameState.Running)
  def print: String = {
    running.gameBoard.fields.zipWithIndex.map { (row, rowIndex) =>
      row.zipWithIndex.map { (maybePlayerId, columnIndex) =>
        maybePlayerId.map { playerId =>
          if running.position.x == columnIndex && running.position.y == rowIndex then
            s"($playerId)"
          else if running.playerPositions.get(playerId).exists { case Position(x, y) => x == columnIndex && y == rowIndex } then
            s"[$playerId]"
          else
            s" $playerId "
        }.getOrElse(" . ")
      }.mkString("")
    }.mkString("\n")
  }

case class Position(x: Int, y: Int) {
  def addOffset(dx: Int, dy: Int, gameBoardWidth: Int, gameBoardHeight: Int): Position = {
    val newX = (x + dx) %% gameBoardWidth
    val newY = (y + dy) %% gameBoardHeight
    Position(newX, newY)
  }
}

case class GameBoard(width: Int, height: Int, fields: Array[Array[Option[String]]]) {
  def updatePlayerPosition(playerId: String, x: Int, y: Int): GameBoard = {
    val newPlayerPositions = fields
    newPlayerPositions(y)(x) = Some(playerId)
    copy(fields = newPlayerPositions)
  }

  def removePlayers(playerIds: List[String]): GameBoard = {
    val newPlayerPositions = fields
    for {
      y <- 0 until height
      x <- 0 until width
    } {
      if (newPlayerPositions(y)(x).exists(playerIds.contains)) newPlayerPositions(y)(x) = None
    }
    copy(fields = newPlayerPositions)
  }

  def playerAt(position: Position): Option[String] = {
    fields(position.y)(position.x)
  }

  def playerLeftOf(position: Position): Option[String] = {
    val x = (position.x - 1) %% width
    val y = position.y
    val res = fields(y)(x)
    res
  }

  def playerRightOf(position: Position): Option[String] = {
    val x = (position.x + 1) %% width
    val y = position.y
    fields(y)(x)
  }

  def playerTopOf(position: Position): Option[String] = {
    val x = position.x
    val y = (position.y - 1) %% height
    fields(y)(x)
  }

  def playerBottomOf(position: Position): Option[String] = {
    val x = position.x
    val y = (position.y + 1) %% height
    fields(y)(x)
  }
}

object GameBoard {
  def apply(width: Int, height: Int): GameBoard = {
    val playerPositions = Array.fill[Option[String]](height, width)(None)
    GameBoard(width, height, playerPositions)
  }
}

val chatMessages: List[String] = List(
  "You will all die",
  "I'm comming after all of you",
  "I will kill you all",
  "All of you are doomed",
  "The end is near"
)

extension (a: Int)
  def %%(b: Int): Int = ((a % b) + b) % b

var lastTickTimestamp: Long = System.currentTimeMillis()
