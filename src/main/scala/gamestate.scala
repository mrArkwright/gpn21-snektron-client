enum GameState:
  case Idle

  case Starting(
    width: Int,
    height: Int,
    playerId: String,
    playerPositions: Map[String, Position] = Map.empty
  )

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
}

object GameBoard {
  def apply(width: Int, height: Int): GameBoard = {
    val playerPositions = Array.fill[Option[String]](height, width)(None)
    GameBoard(width, height, playerPositions)
  }
}
