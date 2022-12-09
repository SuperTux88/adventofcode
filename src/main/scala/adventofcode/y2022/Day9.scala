package adventofcode.y2022

import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day9 extends Year2022 {
  override val day = 9

  private val MotionRE = """(\w) ([\d]+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val motions = input.getLines().map {
      case MotionRE(dir, dist) => Motion(dir.charAt(0), dist.toInt)
    }.toSeq

    printDayPart(1, moveRope(motions).size, "The tail visited %s positions")
    printDayPart(2, moveRope(motions, 10).size, "The tail of the longer rope visited %s positions")
  }

  // returns the positions of the tail
  private def moveRope(motions: Seq[Motion], length: Int = 2): Set[Pos] =
    motions.foldLeft(Set(Pos.zero), List.fill(length)(Pos.zero)) {
      case ((tailVisited, rope), motion) =>
        (0 until motion.dist).foldLeft(tailVisited, rope) {
          case ((visited, rope), _) =>
            val newRope = rope.drop(1).foldLeft(List(rope.head.move(motion.dir))) {
              case (newRope, current) => current + moveTail(newRope.head, current) :: newRope
            }
            (visited + newRope.head, newRope.reverse)
        }
    }._1

  private def moveTail(head: Pos, tail: Pos) =
    if (head.neighbors.contains(tail) || head == tail) {
      Pos.zero
    } else {
      val diff = head - tail
      val moveX = if (diff.x > 0) 1 else if (diff.x < 0) -1 else 0
      val moveY = if (diff.y > 0) 1 else if (diff.y < 0) -1 else 0
      Pos(moveX, moveY)
    }

  private case class Motion(dir: Char, dist: Int)
}
