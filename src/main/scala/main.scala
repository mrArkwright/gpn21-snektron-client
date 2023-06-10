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

def sendMessage(message: ClientMessage, outputStream: DataOutputStream): Unit = {
  val messageString = message.toProtocolMessage
  outputStream.writeBytes(messageString + "\n")
  outputStream.flush()

  val currentTimestamp = System.currentTimeMillis()
  val timeDifference = currentTimestamp - lastTickTimestamp

  println(s"Sent: $messageString, timeDifference: $timeDifference")
}

