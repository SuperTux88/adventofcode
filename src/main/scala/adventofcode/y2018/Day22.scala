package adventofcode.y2018

import adventofcode.Logging
import adventofcode.common.Dijkstra

import scala.collection.mutable

object Day22 extends Year2018 {
  override val day = 22

  private val lines = input.getLines.toList
  private val depth = lines.head.split(" ")(1).toInt
  private val Array(targetX, targetY) = lines(1).split(" ")(1).split(",").map(_.toInt)
  private val target = Pos(targetX, targetY)

  private val map = mutable.Map.empty[Pos, Region]

  Seq(Pos(0, 0), target, target.left, target.up).foreach(getRegion)

  printMap()

  printDayPart(1, map.map(_._2.riskLevel).sum)

  private val (duration, _) = Dijkstra(
    State(Pos(0, 0), Torch()),
    State(target, Torch()),
    {state: State => state.neighbors()}
  )

  printDayPart(2, duration)

  private def getRegion(pos: Pos): Region = map.getOrElseUpdate(pos, calculateRegion(pos.x, pos.y))

  private def calculateRegion(x: Int, y: Int): Region = {
    val geologicIndex = Pos(x, y) match {
      case Pos(0, 0) => 0
      case pos if pos == target => 0
      case Pos(_, 0) => x * 16807
      case Pos(0, _) => y * 48271
      case pos => getRegion(pos.left).erosionLevel * getRegion(pos.up).erosionLevel
    }
    val erosionLevel = (geologicIndex + depth) % 20183
    Region(geologicIndex, erosionLevel)
  }

  private case class Pos(x: Int, y: Int) {
    def +(direction: (Int, Int)): Pos = Pos(x + direction._1, y + direction._2)
    def up: Pos = copy(y = y - 1)
    def left: Pos = copy(x = x - 1)
  }

  private object Pos {
    val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))
  }

  private case class Region(geologicIndex: Int, erosionLevel: Int) {
    def riskLevel: Int = erosionLevel % 3
    def regionType: RegionType = {
      riskLevel match {
        case 0 => Rocky()
        case 1 => Wet()
        case 2 => Narrow()
      }
    }
  }

  private case class State(pos: Pos, tool: Tool) {
    def neighbors(): List[(Int, State)] = {
      val newTool = Seq(Torch(), ClimbingGear(), Neither()).find {
        t => t != tool && getRegion(pos).regionType.valid(t)
      }.get

      (7, State(pos, newTool)) :: Pos.directions.map(pos + _).filter { newPos =>
        newPos.x >= 0 && newPos.y >= 0 && getRegion(newPos).regionType.valid(tool)
      }.map(newPos => (1, State(newPos, tool)))
    }
  }

  sealed trait Tool
  private case class Torch() extends Tool
  private case class ClimbingGear() extends Tool
  private case class Neither() extends Tool

  sealed trait RegionType {
    def valid(tool: Tool): Boolean
  }
  private case class Rocky() extends RegionType {
    override def valid(tool: Tool): Boolean = tool != Neither()
    override def toString: String = "."
  }
  private case class Wet() extends RegionType {
    override def valid(tool: Tool): Boolean = tool != Torch()
    override def toString: String = "="
  }
  private case class Narrow() extends RegionType {
    override def valid(tool: Tool): Boolean = tool != ClimbingGear()
    override def toString: String = "|"
  }

  private def printMap(): Unit = if (Logging.debug) {
    (0 to targetY).foreach { y =>
      println((0 to targetX).map(x => map(Pos(x, y)).regionType).mkString)
    }
  }
}
