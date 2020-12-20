package adventofcode.y2020

import adventofcode.common.pos.Direction.DirectionPos
import adventofcode.common.pos.Pos

import scala.annotation.tailrec

object Day20 extends Year2020 {
  override val day = 20

  private val TileHeaderRE = """Tile (\d+):""".r

  private val tiles = inputString.split("\n\n").map { tile =>
    val tileIter = tile.linesIterator
    val tileId = tileIter.next() match {
      case TileHeaderRE(id) => id.toLong
    }
    tileId -> Tile(tileId, Grid(Pos.parseMap(tileIter, char => char == '#')))
  }.toMap

  private val tilesNeighbors = tiles.view.mapValues { tile =>
    (tiles - tile.id).filter(other => tile.isNeighbor(other._2)).keys.toSeq
  }.toMap

  printDayPart(1, tilesNeighbors.filter(_._2.size == 2).keys.product, "product of corner tiles: %s")

  private val topLeftMap = Map(Pos.zero -> rotateCorner(tiles(tilesNeighbors.find(_._2.size == 2).get._1), Set(1, 2)))
  private val fullTilesMap = findGrid(topLeftMap)
  private val fullImageMap = fullTilesMap.flatMap {
    case (tilePos, tile) => tile.center.map {
      case (pos, bool) => tilePos * (tile.grid.lastIndex - 1) + pos - Pos(1, 1) -> bool
    }
  }

  private val monsterString =
    """                  #
      |#    ##    ##    ###
      | #  #  #  #  #  #
      |""".stripMargin
  private val monsterPositions = Pos.parseMap(monsterString.linesIterator, char => char == '#').filter(_._2).keySet
  private val (monsterWidth, monsterHeight) = (monsterPositions.maxBy(_.x).x, monsterPositions.maxBy(_.y).y)

  private val monsterCount = findMonsters(Grid(fullImageMap))

  printDayPart(2, fullImageMap.count(_._2) - monsterCount * monsterPositions.size, "water roughness: %s")

  @tailrec
  private def rotateCorner(tile: Tile, targetBorder: Set[Int]): Tile =
    if (tile.neighborsDirections(tiles, tilesNeighbors).filter(_._2 > 0).keySet == targetBorder)
      tile
    else
      rotateCorner(tile.rotateClockwise, targetBorder)

  @tailrec
  private def rotateAndFlip(tile: Tile, targetNeighbors: Map[Int, Long], step: Int = 0): Tile = {
    val neighbors = tile.neighborsDirections(tiles, tilesNeighbors)
    if (targetNeighbors.forall(t => neighbors.getOrElse(t._1, -1) == t._2))
      tile
    else if (step == 3)
      rotateAndFlip(tile.flipHorizontal, targetNeighbors, step + 1)
    else
      rotateAndFlip(tile.rotateClockwise, targetNeighbors, step + 1)
  }

  @tailrec
  private def findGrid(tilesMap: Map[Pos, Tile], currentPos: Pos = Pos(1, 0)): Map[Pos, Tile] = {
    val (nextTileId, targetNeighbors) =
      if (currentPos.y == 0) {
        val leftTile = tilesMap(currentPos.left)
        (leftTile.neighborsDirections(tiles, tilesNeighbors)(1), Map(0 -> -1L, 3 -> leftTile.id))
      } else {
        val upTile = tilesMap(currentPos.up)
        val leftNeighbor = if (currentPos.x == 0) -1L else tilesMap(currentPos.left).id
        (upTile.neighborsDirections(tiles, tilesNeighbors)(2), Map(0 -> upTile.id, 3 -> leftNeighbor))
      }
    val rotatedNextTile = rotateAndFlip(tiles(nextTileId), targetNeighbors)
    val newTilesMap = tilesMap.updated(currentPos, rotatedNextTile)
    if (rotatedNextTile.neighborsDirections(tiles, tilesNeighbors)(1) > 0)
      findGrid(newTilesMap, currentPos.right)
    else if (rotatedNextTile.neighborsDirections(tiles, tilesNeighbors)(2) > 0)
      findGrid(newTilesMap, Pos(0, currentPos.y + 1))
    else
      newTilesMap
  }

  @tailrec
  private def findMonsters(fullImage: Grid, step: Int = 0): Int = {
    val monsters = countMonsters(fullImage)
    if (monsters > 0)
      monsters
    else if (step == 3)
      findMonsters(fullImage.flipHorizontal, step + 1)
    else
      findMonsters(fullImage.rotateClockwise, step + 1)
  }

  private def countMonsters(fullImage: Grid): Int =
    (0 until fullImage.lastIndex - monsterHeight).reduce { (sum, y) =>
      sum + (0 until fullImage.lastIndex - monsterWidth).count { x =>
        monsterPositions.forall(p => fullImage.positions(Pos(x, y) + p))
      }
    }

  private case class Grid(positions: Map[Pos, Boolean]) {
    val lastIndex: Int = positions.maxBy(_._1.x)._1.x

    def flipHorizontal: Grid = copy(positions = positions.map {
      case (pos, bool) => pos.copy(x = -pos.x + lastIndex) -> bool
    })

    def rotateClockwise: Grid = copy(positions = positions.map {
      case (pos, bool) =>
        val rotated = pos.rotateRight
        rotated.copy(x = rotated.x + lastIndex) -> bool
    })

    def print(): Unit =
      (0 to lastIndex).foreach { y =>
        println((0 to lastIndex).map(x => if (positions(Pos(x, y))) '#' else '.').mkString)
      }
  }

  private case class Tile(id: Long, grid: Grid) {
    // top, right, bottom, left
    def getBorder(border: Int): Seq[Boolean] = border match {
      case 0 => (0 to grid.lastIndex).map(x => grid.positions(Pos(x, 0)))
      case 1 => (0 to grid.lastIndex).map(y => grid.positions(Pos(grid.lastIndex, y)))
      case 2 => (0 to grid.lastIndex).map(x => grid.positions(Pos(x, grid.lastIndex)))
      case 3 => (0 to grid.lastIndex).map(y => grid.positions(Pos(0, y)))
    }
    def getBorders: Seq[Seq[Boolean]] = (0 to 3).map(getBorder)

    def isNeighbor(tile: Tile): Boolean = tile.getBorders.exists(isNeighbor)
    def isNeighbor(edge: Seq[Boolean]): Boolean = getBorders.contains(edge) || getBorders.contains(edge.reverse)

    def neighborsDirections(tiles: Map[Long, Tile], neighborsMap: Map[Long, Seq[Long]]): Map[Int, Long] =
      getBorders.zipWithIndex.map {
        case (border, index) => index -> neighborsMap(id).map(tiles(_)).find(_.isNeighbor(border)).map(_.id).getOrElse(-1L)
      }.toMap

    def flipHorizontal: Tile = copy(grid = grid.flipHorizontal)
    def rotateClockwise: Tile = copy(grid = grid.rotateClockwise)

    def center: Map[Pos, Boolean] = grid.positions.filter {
      case (pos, _) => pos.x != 0 && pos.y != 0 && pos.x != grid.lastIndex && pos.y != grid.lastIndex
    }
  }
}
