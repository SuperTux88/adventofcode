package adventofcode.common.pos

object Direction {
  // up, right, down, left
  val directions: List[(Int, Int)] = List((0, -1), (1, 0), (0, 1), (-1, 0))
  // top-left, top-right, down-right, down-left
  val diagonals: List[(Int, Int)] = List((-1, -1), (1, -1), (1, 1), (-1, 1))
  val directionsWithDiagonals: List[(Int, Int)] = directions ::: diagonals

  val directions3d: List[(Int, Int, Int)] = List((-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1))

  val compassDirections: List[Char] = List('N', 'E', 'S', 'W')

  val up: Pos = Pos(0, -1)
  val right: Pos = Pos(1, 0)
  val down: Pos = Pos(0, 1)
  val left: Pos = Pos(-1, 0)
  
  val upIndex = 0
  val rightIndex = 1
  val downIndex = 2
  val leftIndex = 3

  def rotateLeft(directionIndex: Int): Int = (directionIndex + 3) % 4
  def rotateRight(directionIndex: Int): Int = (directionIndex + 1) % 4
  def flip(directionIndex: Int): Int = (directionIndex + 2) % 4

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

  implicit class DirectionPos3D(pos: Pos3D) {
    def rotateLeft: Pos3D = Pos3D(pos.z, pos.y, -pos.x)
    def rotateRight: Pos3D = Pos3D(-pos.z, pos.y, pos.x)
    def tiltLeft: Pos3D = Pos3D(pos.y, -pos.x, pos.z)
    def tiltRight: Pos3D = Pos3D(-pos.y, pos.x, pos.z)
    def tiltForward: Pos3D = Pos3D(pos.x, -pos.z, pos.y)
    def tiltBackwards: Pos3D = Pos3D(pos.x, pos.z, -pos.y)

    // rotate around 0,0,0 - only supports 90 degree steps
    def rotate(index: Int): Pos3D = {
      val tilted = index % 6 match { // tilt/flip in 6 directions
        case 0 => pos
        case 1 => tiltRight
        case 2 => Pos3D(-pos.x, -pos.y, pos.z)
        case 3 => tiltLeft
        case 4 => tiltForward
        case 5 => tiltBackwards
      }
      index / 6 % 4 match { // rotate in all 4 directions
        case 0 => tilted
        case 1 => tilted.rotateRight
        case 2 => Pos3D(-tilted.x, tilted.y, -tilted.z)
        case 3 => tilted.rotateLeft
      }
    }
  }
}
