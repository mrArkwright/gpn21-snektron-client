import java.io.*
import java.net.Socket
import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

var lastTickTimestamp: Long = System.currentTimeMillis()

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
      gameState match
        case startingGameSate: GameState.Starting =>
          val newPlayerPositions = startingGameSate.playerPositions + (playerId -> Position(x, y))
          startingGameSate.copy(playerPositions = newPlayerPositions)

        case runningGameState: GameState.Running =>
          val newGameBoard = runningGameState.gameBoard.updatePlayerPosition(playerId, x, y)
          val newPlayerPositions = runningGameState.playerPositions + (playerId -> Position(x, y))
          val newPosition = if (playerId == runningGameState.playerId) Position(x, y) else runningGameState.position
          runningGameState.copy(position = newPosition, gameBoard = newGameBoard, playerPositions = newPlayerPositions)

        case _ =>
          gameState

    case ServerMessage.Tick =>
      gameState match
        case GameState.Starting(width, height, playerId, playerPositions) =>
          println("*******************************")
          val gameBoard = GameBoard(width, height)

          val gameBoardWithPlayers = playerPositions.foldLeft(gameBoard) { case (gameBoard, (playerId, position)) =>
            gameBoard.updatePlayerPosition(playerId, position.x, position.y)
          }

          val position = playerPositions(playerId)

          val runningGameState = GameState.Running(playerId, position, gameBoardWithPlayers, playerPositions)
          handleServerMessage(runningGameState, message, outputStream)

        case runningGameState: GameState.Running =>
          println("-------------------")
          println(runningGameState.print)

          val nextDirection = computeNextDirection(runningGameState)

          println(s"playerId: ${runningGameState.playerId}")
          println(s"position: ${runningGameState.position}")
          println(s"nextDirection: $nextDirection")
          println("-------------------")

          sendMessage(ClientMessage.Move(nextDirection), outputStream)

          runningGameState.copy(lastDirection = Some(nextDirection))

        case _ =>
          gameState

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

    case ServerMessage.Unknown(message) =>
      gameState

def sendMessage(message: ClientMessage, outputStream: DataOutputStream): Unit = {
  val messageString = message.toProtocolMessage
  outputStream.writeBytes(messageString + "\n")
  outputStream.flush()

  val currentTimestamp = System.currentTimeMillis()
  val timeDifference = currentTimestamp - lastTickTimestamp

  println(s"Sent: $messageString, timeDifference: $timeDifference")
}
