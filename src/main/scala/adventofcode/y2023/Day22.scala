package adventofcode.y2023

import adventofcode.common.pos.{Pos, Pos3D}

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day22 extends Year2023 {
  override val day = 22

  override def runDay(input: BufferedSource): Unit = {
    val bricks = input.getLines().takeWhile(_.nonEmpty).zipWithIndex.map { (brick, id) =>
      val Array(a, b) = brick.split("~").map { pos =>
        val Array(x, y, z) = pos.split(",").map(_.toInt)
        Pos3D(x, y, z)
      }
      Brick(id, a, b)
    }.toList

    val ground = bricks.flatMap(_.area).toSet.map(p => Pos3D(p.x, p.y, 0))

    val settled = bricks.sortBy(_.bottom.z).foldLeft(ground, List.empty[Brick]) { case ((settled, bricks), brick) =>
      val settledBrick = brick.fall(settled)
      (settled ++ settledBrick.volume, settledBrick :: bricks)
    }._2.toSet

    val byBottom = settled.groupBy(_.bottom.z)
    val byTop = settled.groupBy(_.top.z)

    val canBeDisintegrated = byTop.flatMap { (z, bricks) =>
      byBottom.get(z + 1) match {
        case Some(topBricks) =>
          if (bricks.size == 1)
            Nil // if it's the only brick on this layer, it can't be removed
          else
            bricks.filter(b => {
              val others = bricks - b
              // check if all bricks above are still supported by the remaining bricks
              topBricks.forall(_.isSupportedBy(others))
            })
        case None => bricks // top layer can be removed
      }
    }.toSet

    printDayPart(1, canBeDisintegrated.size, "Number of bricks which can be safely disintegrated: %s")

    val bricksForChainReaction = settled.diff(canBeDisintegrated)
    val willFall = bricksForChainReaction.toSeq.par.map(react(_, byBottom, byTop))

    printDayPart(2, willFall.map(_.size).sum, "Sum of numbers of bricks that would fall: %s")
  }

  private def react(brick: Brick, byBottom: Map[Int, Set[Brick]], byTop: Map[Int, Set[Brick]]) = {
    @tailrec
    def inner(z: Int, byTop: Map[Int, Set[Brick]], fallen: Set[Int] = Set.empty): Set[Int] =
      byBottom.get(z + 1) match {
        case None => fallen
        case Some(topBricks) =>
          val bottomBricks = byTop(z)
          val willFall = topBricks.filterNot(_.isSupportedBy(bottomBricks))
          val newByTop = willFall.foldLeft(byTop) { case (byTop, brick) =>
            byTop.updated(brick.top.z, byTop(brick.top.z) - brick)
          }
          inner(z + 1, newByTop, fallen ++ willFall.map(_.id))
      }

    inner(brick.top.z, byTop.updated(brick.top.z, byTop(brick.top.z) - brick))
  }

  private case class Brick(id: Int, bottom: Pos3D, top: Pos3D) {
    val area: Set[Pos] = (bottom.x to top.x).flatMap { x =>
      (bottom.y to top.y).map { y => Pos(x, y) }
    }.toSet
    private def height = top.z - bottom.z + 1

    def volume: Set[Pos3D] = (bottom.x to top.x).flatMap { x =>
      (bottom.y to top.y).flatMap { y =>
        (bottom.z to top.z).map { z => Pos3D(x, y, z) }
      }
    }.toSet

    def fall(settled: Set[Pos3D]): Brick = {
      val settledHeight = settled.filter(p => area.contains(Pos(p.x, p.y))).maxBy(_.z).z
      copy(bottom = Pos3D(bottom.x, bottom.y, settledHeight + 1), top = Pos3D(top.x, top.y, settledHeight + height))
    }

    def isSupportedBy(others: Set[Brick]): Boolean = others.exists(_.area.intersect(area).nonEmpty)
  }
}
