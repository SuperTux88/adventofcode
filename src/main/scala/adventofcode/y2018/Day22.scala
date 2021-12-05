package adventofcode.y2018

import adventofcode.Logging
import adventofcode.common.pos.Pos
import adventofcode.common.search.AStar

import scala.collection.mutable
import scala.io.BufferedSource

object Day22 extends Year2018 {
  override val day = 22

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines().toList
    val depth = lines.head.split(" ")(1).toInt
    val Array(targetX, targetY) = lines(1).split(" ")(1).split(",").map(_.toInt)
    val target = Pos(targetX, targetY)

    val cave = Cave(mutable.Map.empty[Pos, Region], depth, target)

    Seq(Pos.zero, target, target.left, target.up).foreach(getRegion(_)(cave))

    if (Logging.debug) Pos.printMap(cave.map.toMap, _.regionType.asChar)

    printDayPart(1, cave.map.map(_._2.riskLevel).sum)

    val duration = AStar(
      State(Pos.zero, Torch),
      (state: State) => state == State(target, Torch),
      (state: State) => state.neighbors(cave)
    )._1

    printDayPart(2, duration)
  }

  private def getRegion(pos: Pos)(implicit cave: Cave): Region =
    cave.map.getOrElseUpdate(pos, calculateRegion(pos.x, pos.y))

  private def calculateRegion(x: Int, y: Int)(implicit cave: Cave): Region = {
    val geologicIndex = Pos(x, y) match {
      case Pos.zero => 0
      case pos if pos == cave.target => 0
      case Pos(_, 0) => x * 16807
      case Pos(0, _) => y * 48271
      case pos => getRegion(pos.left).erosionLevel * getRegion(pos.up).erosionLevel
    }
    val erosionLevel = (geologicIndex + cave.depth) % 20183
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
    def neighbors(implicit cave: Cave): List[(Int, Int, State)] = {
      val newTool = Seq(Torch, ClimbingGear, Neither).find {
        t => t != tool && getRegion(pos).regionType.valid(t)
      }.get

      (7, pos.distance(cave.target), State(pos, newTool)) :: pos.directions.filter { newPos =>
        newPos.positive && getRegion(newPos).regionType.valid(tool)
      }.map(newPos => (1, newPos.distance(cave.target), State(newPos, tool)))
    }
  }

  sealed trait Tool
  private case object Torch extends Tool
  private case object ClimbingGear extends Tool
  private case object Neither extends Tool

  sealed trait RegionType {
    def valid(tool: Tool): Boolean
    def asChar: Char
  }
  private case object Rocky extends RegionType {
    override def valid(tool: Tool): Boolean = tool != Neither
    override def asChar: Char = '.'
  }
  private case object Wet extends RegionType {
    override def valid(tool: Tool): Boolean = tool != Torch
    override def asChar: Char = '='
  }
  private case object Narrow extends RegionType {
    override def valid(tool: Tool): Boolean = tool != ClimbingGear
    override def asChar: Char = '|'
  }

  private case class Cave(map: mutable.Map[Pos, Region], depth: Int, target: Pos)
}
