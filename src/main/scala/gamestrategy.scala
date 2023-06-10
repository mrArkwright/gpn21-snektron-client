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
