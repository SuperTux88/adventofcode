package adventofcode.y2019

import adventofcode.common.Pos

object Day3 extends Year2019 {
  override val day = 3

  val wires = input.getLines().take(2).map(_.split(",").map(parseInstruction).toList).toList
  val List(wireA, wireB) = wires.map(generatePath(_))
  val crossings = wireA intersect wireB

  printDayPart(1, crossings.map(_.distance(Pos(0, 0))).min)

  printDayPart(2, crossings.map(crossing => wireA.indexOf(crossing) + wireB.indexOf(crossing) + 2).min)

  private def parseInstruction(instruction: String) =
    instruction.splitAt(1) match {
      case (direction, distance) => (direction.head, distance.toInt)
    }

  private def generatePath(wire: Seq[(Char, Int)]) =
    wire.iterator.flatMap {
      case (direction, distance) =>
        Iterator.fill(distance)(direction)
    }.scanLeft(Pos(0, 0)) {
      (pos, direction) => pos.move(direction)
    }.toList.tail
}
