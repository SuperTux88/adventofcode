package adventofcode.common

import adventofcode.common.pos.PosTrait

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
}
