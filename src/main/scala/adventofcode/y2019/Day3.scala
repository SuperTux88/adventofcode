package adventofcode.y2019

import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day3 extends Year2019 {
  override val day = 3

  override def runDay(input: BufferedSource): Unit = {
    val wires = input.getLines().take(2).map(_.split(",").map(parseInstruction).toList).toList
    val List(wireA, wireB) = wires.map(generatePath(_))
    val crossings = wireA intersect wireB

    printDayPart(1, crossings.map(_.distance(Pos.zero)).min, "manhatten distance to closest intersection: %s")

    val wireDistance = crossings.map(crossing => wireA.indexOf(crossing) + wireB.indexOf(crossing) + 2).min
    printDayPart(2, wireDistance, "wire distance to closest intersection: %s")
  }

  private def parseInstruction(instruction: String) =
    instruction.splitAt(1) match {
      case (direction, distance) => (direction.head, distance.toInt)
    }

  private def generatePath(wire: Seq[(Char, Int)]) =
    wire.iterator.flatMap {
      case (direction, distance) =>
        Iterator.fill(distance)(direction)
    }.scanLeft(Pos.zero) {
      (pos, direction) => pos.move(direction)
    }.toList.tail
}
