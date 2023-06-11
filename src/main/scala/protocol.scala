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
  case Unknown(fields: List[String])

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
    case _ => ServerMessage.Unknown(messageFields)
  }
}
