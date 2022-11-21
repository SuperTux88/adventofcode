package adventofcode.y2021

import adventofcode.common.IterableImplicits
import adventofcode.common.MapImplicits.ListMapImplicits

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParSeq
import scala.io.BufferedSource

object Day12 extends Year2021 {
  override val day = 12

  private val START = Cave("start")
  private val END = Cave("end")

  override def runDay(input: BufferedSource): Unit = {
    val connections = input.getLines().takeWhile(_.nonEmpty).flatMap {
      line => parseConnection(line.split("-").map(parseCave))
    }.foldLeft(Map[Cave, List[Cave]]().withDefaultValue(Nil)) {
      case (conns, (from, to)) => conns.prependWith(from, to)
    }

    printDayPart(1, countPaths(connections), "number of paths without visiting small caves twice: %s")
    printDayPart(2, countPaths(connections, true), "number of paths with visiting one small caves twice: %s")
  }

  private def countPaths(connections: Map[Cave, List[Cave]], allowOneSmallDuplicate: Boolean = false) = {
    @tailrec
    def findPaths(paths: ParSeq[List[Cave]] = ParSeq(List(START))): ParSeq[List[Cave]] = {
      if (paths.forall(_.head == END)) {
        paths
      } else {
        val newPaths = paths.flatMap { path =>
          path.head match {
            case last if last == END => List(path)
            case last =>
              if (allowOneSmallDuplicate && !path.filter(_.small).groupCount(identity).exists(_._2 > 1))
                connections(last).map(_ :: path)
              else
                connections(last).filterNot(c => c.small && path.contains(c)).map(_ :: path)
          }
        }
        findPaths(newPaths)
      }
    }

    findPaths().size
  }

  private def parseCave(name: String) = Cave(name, name == name.toLowerCase)
  private def parseConnection(connection: Array[Cave]) = connection match {
    case conn if conn.contains(START) => Map(START -> conn.find(_ != START).get)
    case conn if conn.contains(END) => Map(conn.find(_ != END).get -> END)
    case Array(from, to) => Map(from -> to, to -> from)
  }

  private case class Cave(name: String, small: Boolean = true)
}
