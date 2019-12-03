package adventofcode.y2019

import adventofcode.common.Pos

import scala.collection.parallel.CollectionConverters._

object Day3 extends Year2019 {
  override val day = 3

  val wires = input.getLines().take(2).map(_.split(",").map(parseInstruction).toList).toList
  val List(wireA, wireB) = wires.par.map(_.foldLeft(Seq(Pos(0, 0)))(move).tail).toList
  val crossings = wireA intersect wireB

  printDayPart(1, crossings.map(_.distance(Pos(0, 0))).min)

  printDayPart(2, crossings.map(crossing => wireA.indexOf(crossing) + wireB.indexOf(crossing) + 2).min)

  private def parseInstruction(instruction: String) = {
    val (direction, distance) = instruction.splitAt(1)
    (direction.head, distance.toInt)
  }

  private def move(positions: Seq[Pos], instruction: (Char, Int)) = {
    val (direction, distance) = instruction

    positions ++ (1 to distance).scanLeft(positions.last)((pos, _) => pos.move(direction)).tail
  }
}
