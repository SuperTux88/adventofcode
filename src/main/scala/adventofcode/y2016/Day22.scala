package adventofcode.y2016

import adventofcode.Logging

object Day22 extends Year2016 {
  override val day = 22

  val NodeRE = """/dev/grid/node-x(\d+)-y(\d+) +(\d+)T +(\d+)T +(\d+)T +(\d+)%""".r

  private val grid = input.getLines().flatMap {
    case NodeRE(x, y, size, used, avail, use) =>
      Some(Node(x.toInt, y.toInt, size.toInt, used.toInt, avail.toInt, use.toInt))
    case _ => None
  }.toList

  printDayPart(1, countPairs(grid))

  val (width, height) = (grid.map(_.x).max + 1, grid.map(_.y).max + 1)
  private val emptyNode = grid.find(_.isEmpty).get
  private val gridMap = grid.map(node => (node.x, node.y) -> node).toMap

  val topX = toTop(emptyNode)
  val stepsToTop = emptyNode.x - topX + emptyNode.y
  val shortestPath = stepsToTop + width - 1 - topX + 5 * (width - 2)

  printDayPart(2, shortestPath)

  if (Logging.debug) printMap()

  private case class Node(x: Int, y: Int, size: Int, used: Int, avail: Int, use: Int) {
    def isEmpty: Boolean = used == 0
    def isWall: Boolean = used > emptyNode.size

    def mapChar: Char =
      if (isEmpty) '_'
      else if (isWall) '#'
      else if (x == width-1 && y == 0) 'G'
      else '.'
  }

  private def countPairs(nodes: List[Node]) =
    nodes.count { node => node.used != 0 && nodes.exists(n => n.avail > node.used) }

  private def toTop(node: Node) = {
    var (x, y) = (node.x, node.y)
    while (!gridMap(x, y - 1).isWall) y -= 1
    while (gridMap(x, y - 1).isWall) x -= 1
    x
  }

  private def printMap() =
    (0 until height).foreach { y =>
      println((0 until width).map(x => gridMap(x, y).mapChar).mkString)
    }
}
