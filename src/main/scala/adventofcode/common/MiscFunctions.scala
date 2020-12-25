package adventofcode.common

import adventofcode.common.pos.PosTrait

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

object MiscFunctions {
  /**
    * Run a number of cycles of Conway's Game of Life
    */
  def conwaysGameOfLife[P <: PosTrait[P], V](map: Set[P],
                                             cyclesToRun: Int,
                                             nextState: (Boolean, V) => Boolean,
                                             nextStateValue: (P, Set[P]) => V = (pos: P, state: Set[P]) => pos.neighbors.count(state.contains).asInstanceOf[V],
                                             nextPositions: (Set[P], Int) => Set[P] = (state: Set[P], _: Int) => state.flatMap(_.neighbors)): Set[P] =
    (1 to cyclesToRun).foldLeft(map) { (state, index) =>
      nextPositions(state, index).par.flatMap { pos =>
        if (nextState(state.contains(pos), nextStateValue(pos, state))) Some(pos) else None
      }.seq
    }

  /**
    * From a map with multiple possibilities for keys, find unique value for each key
    */
  @tailrec
  def reduceToUniqueValues[K, V](map: Map[K, Set[V]]): Map[K, V] = {
    val validForOne = map.filter(_._2.size == 1)
    if (validForOne.size == map.size) {
      validForOne.view.mapValues(_.head).toMap
    } else {
      val reduced = map.view.mapValues { validFor =>
        if (validFor.size == 1) validFor
        else validFor -- validForOne.values.flatten.toSet
      }.toMap
      reduceToUniqueValues(reduced)
    }
  }
}
