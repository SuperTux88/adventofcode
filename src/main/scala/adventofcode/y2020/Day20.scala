package adventofcode.y2020

import adventofcode.common.pos.Direction.DirectionPos
import adventofcode.common.pos.Pos

import scala.annotation.tailrec

object Day20 extends Year2020 {
  override val day = 20

  private val TileHeaderRE = """Tile (\d+):""".r

  override def runDay(input: String): Unit = {
    val tiles = input.split("\n\n").map { tile =>
      val tileIter = tile.linesIterator
      val tileId = tileIter.next() match {
        case TileHeaderRE(id) => id.toLong
      }
      tileId -> Tile(tileId, Grid(Pos.parseMap(tileIter, char => char == '#')))
    }.toMap

    val tilesNeighbors = tiles.view.mapValues { tile =>
      (tiles - tile.id).filter(other => tile.isNeighbor(other._2)).keys.toSeq
    }.toMap

    printDayPart(1, tilesNeighbors.filter(_._2.size == 2).keys.product, "product of corner tiles: %s")

    val tilesData = TilesData(tiles, tilesNeighbors)
    val topLeftMap = Map(Pos.zero -> rotateCorner(tilesData, tiles(tilesNeighbors.find(_._2.size == 2).get._1), Set(1, 2)))
    val fullTilesMap = findGrid(tilesData, topLeftMap)
    val fullImageMap = fullTilesMap.flatMap {
      case (tilePos, tile) => tile.center.map {
        case (pos, bool) => tilePos * (tile.grid.lastIndex - 1) + pos - Pos(1, 1) -> bool
      }
    }

    val monsterCount = Monster.findMonsters(Grid(fullImageMap))
    printDayPart(2, fullImageMap.count(_._2) - monsterCount * Monster.positions.size, "water roughness: %s")
  }

  @tailrec
  private def rotateCorner(tilesData: TilesData, tile: Tile, targetBorder: Set[Int]): Tile =
    if (tile.neighborsDirections(tilesData).filter(_._2 > 0).keySet == targetBorder)
      tile
    else
      rotateCorner(tilesData, tile.rotateClockwise, targetBorder)

  @tailrec
  private def rotateAndFlip(tilesData: TilesData, tile: Tile, targetNeighbors: Map[Int, Long], step: Int = 0): Tile = {
    val neighbors = tile.neighborsDirections(tilesData)
    if (targetNeighbors.forall(t => neighbors.getOrElse(t._1, -1) == t._2))
      tile
    else if (step == 3)
      rotateAndFlip(tilesData, tile.flipHorizontal, targetNeighbors, step + 1)
    else
      rotateAndFlip(tilesData, tile.rotateClockwise, targetNeighbors, step + 1)
  }

  @tailrec
  private def findGrid(tilesData: TilesData, tilesMap: Map[Pos, Tile], currentPos: Pos = Pos(1, 0)): Map[Pos, Tile] = {
    val (nextTileId, targetNeighbors) =
      if (currentPos.y == 0) {
        val leftTile = tilesMap(currentPos.left)
        (leftTile.neighborsDirections(tilesData)(1), Map(0 -> -1L, 3 -> leftTile.id))
      } else {
        val upTile = tilesMap(currentPos.up)
        val leftNeighbor = if (currentPos.x == 0) -1L else tilesMap(currentPos.left).id
        (upTile.neighborsDirections(tilesData)(2), Map(0 -> upTile.id, 3 -> leftNeighbor))
      }
    val rotatedNextTile = rotateAndFlip(tilesData, tilesData.tiles(nextTileId), targetNeighbors)
    val newTilesMap = tilesMap.updated(currentPos, rotatedNextTile)
    if (rotatedNextTile.neighborsDirections(tilesData)(1) > 0)
      findGrid(tilesData, newTilesMap, currentPos.right)
    else if (rotatedNextTile.neighborsDirections(tilesData)(2) > 0)
      findGrid(tilesData, newTilesMap, Pos(0, currentPos.y + 1))
    else
      newTilesMap
  }

  private object Monster {
    private val string =
      """                  #
        |#    ##    ##    ###
        | #  #  #  #  #  #
        |""".stripMargin
    val positions: Set[Pos] = Pos.parseMap(string.linesIterator, char => char == '#').filter(_._2).keySet
    val (width, height) = (positions.maxBy(_.x).x, positions.maxBy(_.y).y)

    @tailrec
    def findMonsters(fullImage: Grid, step: Int = 0): Int = {
      val monsters = countMonsters(fullImage)
      if (monsters > 0)
        monsters
      else if (step == 3)
        findMonsters(fullImage.flipHorizontal, step + 1)
      else
        findMonsters(fullImage.rotateClockwise, step + 1)
    }

    private def countMonsters(fullImage: Grid): Int =
      (0 until fullImage.lastIndex - height).reduce { (sum, y) =>
        sum + (0 until fullImage.lastIndex - width).count { x =>
          positions.forall(p => fullImage.positions(Pos(x, y) + p))
        }
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

    def neighborsDirections(tilesData: TilesData): Map[Int, Long] =
      getBorders.zipWithIndex.map {
        case (border, index) =>
          index -> tilesData.neighborsMap(id).map(tilesData.tiles(_)).find(_.isNeighbor(border)).map(_.id).getOrElse(-1L)
      }.toMap

    def flipHorizontal: Tile = copy(grid = grid.flipHorizontal)
    def rotateClockwise: Tile = copy(grid = grid.rotateClockwise)

    def center: Map[Pos, Boolean] = grid.positions.filter {
      case (pos, _) => pos.x != 0 && pos.y != 0 && pos.x != grid.lastIndex && pos.y != grid.lastIndex
    }
  }

  private case class TilesData(tiles: Map[Long, Tile], neighborsMap: Map[Long, Seq[Long]])
}
