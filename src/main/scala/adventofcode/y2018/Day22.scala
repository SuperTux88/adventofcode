package adventofcode.y2018

import adventofcode.Logging
import adventofcode.common.pos.Pos
import adventofcode.common.search.AStar

import scala.collection.mutable

object Day22 extends Year2018 {
  override val day = 22

  private val lines = input.getLines.toList
  private val depth = lines.head.split(" ")(1).toInt
  private val Array(targetX, targetY) = lines(1).split(" ")(1).split(",").map(_.toInt)
  private val target = Pos(targetX, targetY)

  private val map = mutable.Map.empty[Pos, Region]

  Seq(Pos.zero, target, target.left, target.up).foreach(getRegion)

  printMap()

  printDayPart(1, map.map(_._2.riskLevel).sum)

  private val duration = AStar(
    State(Pos.zero, Torch),
    {state: State => state == State(target, Torch)},
    {state: State => state.neighbors()}
  )._1

  printDayPart(2, duration)

  private def getRegion(pos: Pos): Region = map.getOrElseUpdate(pos, calculateRegion(pos.x, pos.y))

  private def calculateRegion(x: Int, y: Int): Region = {
    val geologicIndex = Pos(x, y) match {
      case Pos.zero => 0
      case pos if pos == target => 0
      case Pos(_, 0) => x * 16807
      case Pos(0, _) => y * 48271
      case pos => getRegion(pos.left).erosionLevel * getRegion(pos.up).erosionLevel
    }
    val erosionLevel = (geologicIndex + depth) % 20183
    Region(geologicIndex, erosionLevel)
  }

  private case class Region(geologicIndex: Int, erosionLevel: Int) {
    def riskLevel: Int = erosionLevel % 3
    def regionType: RegionType = {
      riskLevel match {
        case 0 => Rocky
        case 1 => Wet
        case 2 => Narrow
      }
    }
  }

  private case class State(pos: Pos, tool: Tool) {
    def neighbors(): List[(Int, Int, State)] = {
      val newTool = Seq(Torch, ClimbingGear, Neither).find {
        t => t != tool && getRegion(pos).regionType.valid(t)
      }.get

      (7, pos.distance(target), State(pos, newTool)) :: Pos.directions.map(pos + _).filter { newPos =>
        newPos.positive && getRegion(newPos).regionType.valid(tool)
      }.map(newPos => (1, newPos.distance(target), State(newPos, tool)))
    }
  }

  sealed trait Tool
  private case object Torch extends Tool
  private case object ClimbingGear extends Tool
  private case object Neither extends Tool

  sealed trait RegionType {
    def valid(tool: Tool): Boolean
  }
  private case object Rocky extends RegionType {
    override def valid(tool: Tool): Boolean = tool != Neither
    override def toString: String = "."
  }
  private case object Wet extends RegionType {
    override def valid(tool: Tool): Boolean = tool != Torch
    override def toString: String = "="
  }
  private case object Narrow extends RegionType {
    override def valid(tool: Tool): Boolean = tool != ClimbingGear
    override def toString: String = "|"
  }

  private def printMap(): Unit = if (Logging.debug) {
    (0 to targetY).foreach { y =>
      println((0 to targetX).map(x => map(Pos(x, y)).regionType).mkString)
    }
  }
}
