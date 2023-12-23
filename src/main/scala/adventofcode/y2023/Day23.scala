package adventofcode.y2023

import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.BufferedSource

object Day23 extends Year2023 {
  override val day = 23

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), identity).withDefaultValue('#')
    val start = Pos.zero.right
    val target = map.keys.max.left

    val branches = findBranches(map) + start + target
    val branchPaths = branches.flatMap(findPathsToNextBranch(map, _, branches)).groupBy(_._1)

    val paths = findPaths(branchPaths, start, target)
    printDayPart(1, paths.max, "Longest possible hike: %s")

    val paths2 = findPaths(branchPaths, start, target, true)
    printDayPart(2, paths2.max, "Longest possible hike with climbing slopes: %s")
  }

  private def findBranches(map: Map[Pos, Char]): Set[Pos] =
    map.keys.filter { pos =>
      pos.directions.count(map(_) == '#') < 2 && map(pos) == '.'
    }.toSet

  private def findPathsToNextBranch(map: Map[Pos, Char], start: Pos, branches: Set[Pos]): List[(Pos, Pos, Boolean, Int)] = {
    @tailrec
    def findPath(current: Pos, path: List[Pos]): List[Pos] = {
      val next = current.directions.find { pos =>
        !path.contains(pos) && map(pos) != '#'
      }.get
      if (branches.contains(next))
        next :: path
      else
        findPath(next, next :: path)
    }

    start.directions.flatMap { first =>
      if (map(first) == '#') None
      else {
        val branch :: path = findPath(first, List(first, start)): @unchecked
        val startSlopePossible = map(first) match {
          case '.' => true
          case '^' if first == start.up => true
          case '>' if first == start.right => true
          case 'v' if first == start.down => true
          case '<' if first == start.left => true
          case _ => false
        }
        val last = path.head
        val endSlopePossible = map(last) match {
          case '.' => true
          case 'v' if last == branch.up => true
          case '<' if last == branch.right => true
          case '^' if last == branch.down => true
          case '>' if last == branch.left => true
          case _ => false
        }
        Some((start, branch, startSlopePossible && endSlopePossible, path.size))
      }
    }
  }

  private def findPaths(branchPaths: Map[Pos, Set[(Pos, Pos, Boolean, Int)]],
                        start: Pos, target: Pos, canHandleSlopes: Boolean = false): List[Int] = {
    def getNeightbors(pos: Pos): List[(Int, Pos)] =
      branchPaths(pos).flatMap {
        case (_, branch, slopePossible, length) =>
          if (slopePossible || canHandleSlopes) {
            Some(length, branch)
          } else None
      }.toList

    // this is a somewhat hacky copy of dijkstra, but without stopping at the first result and without sorting
    // TODO: please refactor and make faster 😘
    def walk(): List[(Int, List[Pos])] = {
      val Q = mutable.Queue((0, List(start)))
      var paths = List.empty[(Int, List[Pos])]

      while (Q.nonEmpty) {
        val (cost, current) = Q.dequeue()
        if (target == current.head) {
          paths = (cost, current) :: paths
        } else {
          getNeightbors(current.head).foreach { n =>
            if (!current.contains(n._2))
              Q.enqueue((cost + n._1, n._2 :: current))
          }
        }
      }

      paths
    }

    walk().map(_._1)
  }
}
