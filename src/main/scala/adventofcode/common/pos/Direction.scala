package adventofcode.common.pos

object Direction {
  // up, right, down, left
  val directions: List[(Int, Int)] = List((0, -1), (1, 0), (0, 1), (-1, 0))
  // top-left, top-right, down-right, down-left
  val diagonals: List[(Int, Int)] = List((-1, -1), (1, -1), (1, 1), (-1, 1))
  val directionsWithDiagonals: List[(Int, Int)] = directions ::: diagonals

  val compassDirections: List[Char] = List('N', 'E', 'S', 'W')

  val up: Pos = Pos(0, -1)
  val right: Pos = Pos(1, 0)
  val down: Pos = Pos(0, 1)
  val left: Pos = Pos(-1, 0)

  implicit class DirectionPos(pos: Pos) {
    def rotateLeft: Pos = Pos(pos.y, -pos.x)
    def rotateRight: Pos = Pos(-pos.y, pos.x)

    // rotate around 0,0 - only supports 90 degree steps
    def rotate(stepsClockwise: Int): Pos =
      stepsClockwise % 4 match {
        case 0 => pos
        case 1 => rotateRight
        case 2 => Pos(-pos.x, -pos.y)
        case 3 => rotateLeft
      }
  }
}
