package adventofcode.y2016

import adventofcode.Logging
import adventofcode.common.pos.Pos

import scala.annotation.unused
import scala.io.BufferedSource

object Day22 extends Year2016 {
  override val day = 22

  private val NodeRE = """/dev/grid/node-x(\d+)-y(\d+) +(\d+)T +(\d+)T +(\d+)T +(\d+)%""".r

  override def runDay(input: BufferedSource): Unit = {
    val grid = input.getLines().flatMap {
      case NodeRE(x, y, size, used, avail, use) =>
        Some(Node(x.toInt, y.toInt, size.toInt, used.toInt, avail.toInt, use.toInt))
      case _ => None
    }.toList

    printDayPart(1, countPairs(grid))

    val (width, height) = (grid.map(_.x).max + 1, grid.map(_.y).max + 1)
    val emptyNode = grid.find(_.isEmpty).get
    val gridMap = grid.map(node => Pos(node.x, node.y) -> node).toMap

    val topX = toTop(gridMap, emptyNode)
    val stepsToTop = emptyNode.x - topX + emptyNode.y
    val shortestPath = stepsToTop + width - 1 - topX + 5 * (width - 2)

    printDayPart(2, shortestPath)

    if (Logging.debug) Pos.printMap(gridMap, _.mapChar(emptyNode, width))
  }

  private case class Node(x: Int, y: Int, size: Int, used: Int, avail: Int, use: Int) {
    def isEmpty: Boolean = used == 0
    def isWall(emptyNode: Node): Boolean = used > emptyNode.size

    @unused
    def mapChar(emptyNode: Node, width: Int): Char =
      if (isEmpty) '_'
      else if (isWall(emptyNode)) '#'
      else if (x == width-1 && y == 0) 'G'
      else '.'
  }

  private def countPairs(nodes: List[Node]) =
    nodes.count { node => node.used != 0 && nodes.exists(n => n.avail > node.used) }

  private def toTop(map: Map[Pos, Node], node: Node) = {
    var (x, y) = (node.x, node.y)
    while (!map(Pos(x, y - 1)).isWall(node)) y -= 1
    while (map(Pos(x, y - 1)).isWall(node)) x -= 1
    x
  }
}
