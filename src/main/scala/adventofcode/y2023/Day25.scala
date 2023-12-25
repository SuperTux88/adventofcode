package adventofcode.y2023

import adventofcode.Logging

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day25 extends Year2023 {
  override val day = 25

  private val ConnectionRE = """(\w+): ([ \w]+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val connections = input.getLines().takeWhile(_.nonEmpty).map {
      case ConnectionRE(a, b) => a -> b.split(" ").toList
    }.toMap
    if (Logging.debug) renderGraph(connections)

    // TODO: find connections to remove
    // nnl -> kpc
    // sph -> rkh
    // hrs -> mnf
    val filtered = connections.map {
      case (k, v) if k == "nnl" => k -> v.filterNot(_ == "kpc")
      case (k, v) if k == "sph" => k -> v.filterNot(_ == "rkh")
      case (k, v) if k == "hrs" => k -> v.filterNot(_ == "mnf")
      case (k, v) => k -> v
    }

    val filteredBothDirections = filtered.foldLeft(filtered) { case (map, (k, v)) =>
      v.foldLeft(map)((map, c) => map.updated(c, k :: map.getOrElse(c, Nil)))
    }

    val side1 = countConnectedComponents(filteredBothDirections, Set(), Set(connections.head._1))
    val side2 = filteredBothDirections.size - side1

    printDayPart(1, side1 * side2, "Product of separated group sizes: %s")
  }

  @tailrec
  private def countConnectedComponents(connections: Map[String, List[String]], seen: Set[String], current: Set[String]): Int =
    if (current.isEmpty) seen.size
    else countConnectedComponents(connections, seen ++ current, current.flatMap(connections).diff(seen))

  private def renderGraph(connections: Map[String, List[String]]): Unit = {
    println("digraph {")
    connections.foreach((k, v) => v.foreach(c => println(s"  $k -> $c")))
    println("}")
  }
}
