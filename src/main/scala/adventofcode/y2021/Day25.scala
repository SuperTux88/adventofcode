package adventofcode.y2021

import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day25 extends Year2021 {
  override val day = 25

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines().takeWhile(_.nonEmpty), identity)
    val size = map.max._1
    val (east, south) = map.filter(_._2 != '.').partition(_._2 == '>')

    printDayPart(1, getSteps(east.keySet, south.keySet, size), "first step on which no cucumbers move: %s")
  }

  private def getSteps(east: Set[Pos], south: Set[Pos], size: Pos): Int = {
    @tailrec
    def step(east: Set[Pos], south: Set[Pos], steps: Int): Int = {
      val newEast = east.map { cucumber =>
        val newCucumber = if cucumber.x == size.x then Pos(0, cucumber.y) else cucumber.right
        if east.contains(newCucumber) || south.contains(newCucumber) then cucumber else newCucumber
      }
      val newSouth = south.map { cucumber =>
        val newCucumber = if cucumber.y == size.y then Pos(cucumber.x, 0) else cucumber.down
        if newEast.contains(newCucumber) || south.contains(newCucumber) then cucumber else newCucumber
      }
      if (south == newSouth && east == newEast)
        steps
      else
        step(newEast, newSouth, steps + 1)
    }

    step(east, south, 0) + 1
  }
}
