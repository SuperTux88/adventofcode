package adventofcode.y2023

import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day3 extends Year2023 {
  override val day = 3

  override def runDay(input: BufferedSource): Unit = {
    val engine = Pos.parseMap(input.getLines(), identity)

    val parts = getPartNumbers(engine)
    printDayPart(1, parts.map(_.number).sum, "Sum of part numbers: %s")

    val gears = parts.filter(_.isGear(engine)).groupBy(_.partPos)
    val ratios = gears.values.filter(_.size == 2).map(_.map(_.number).product)
    printDayPart(2, ratios.sum, "Sum of gear ratios: %s")
  }

  private def getPartNumbers(engine: Map[Pos, Char]): List[PartNumber] = {
    val size = engine.keys.max

    @tailrec
    def checkPos(pos: Pos, numbers: List[PartNumber] = List.empty): List[PartNumber] = {
      if (pos <= size) {
        val (endPos, number) = getPartNumber(engine, pos)
        val nextPos = if (endPos.x >= size.x) Pos(0, endPos.y + 1) else endPos.right
        number match {
          case Some(value) => checkPos(nextPos, value :: numbers)
          case None => checkPos(nextPos, numbers)
        }
      } else numbers
    }

    checkPos(Pos.zero)
  }

  private def getPartNumber(engine: Map[Pos, Char], pos: Pos): (Pos, Option[PartNumber]) =
    readNumber(engine, pos) match {
      case Some(number, endPos) =>
        val numberPositions = pos.lineTo(endPos.left)
        val symbol = numberPositions.flatMap(_.neighbors).find(pos =>
          engine.get(pos) match {
            case Some(char) => char != '.' && !char.isDigit
            case None => false
          })
        symbol match {
          case Some(symbolPos) => (endPos, Some(PartNumber(number, symbolPos)))
          case None => (endPos, None)
        }
      case None => (pos, None)
    }

  @tailrec
  private def readNumber(engine: Map[Pos, Char], pos: Pos, number: Int = 0): Option[(Int, Pos)] =
    engine.get(pos) match {
      case Some(char) if char.isDigit =>
        readNumber(engine, pos.right, number * 10 + engine(pos).asDigit)
      case _ if number > 0 => Some(number, pos)
      case _ => None
    }

  private case class PartNumber(number: Int, partPos: Pos) {
    def isGear(engine: Map[Pos, Char]): Boolean = engine(partPos) == '*'
  }
}
