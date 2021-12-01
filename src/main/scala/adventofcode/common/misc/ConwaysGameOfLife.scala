package adventofcode.common.misc

import adventofcode.common.pos.PosTrait

import scala.collection.parallel.CollectionConverters.*

/**
  * Run a number of cycles of Conway's Game of Life
  */
object ConwaysGameOfLife {
  def run[P <: PosTrait[P], V](map: Set[P],
                               cyclesToRun: Int,
                               nextState: (Boolean, V) => Boolean,
                               nextPositions: (Set[P], Int) => Set[P],
                               nextStateValue: (P, Set[P]) => V = (pos: P, state: Set[P]) => pos.neighbors.count(state.contains).asInstanceOf[V],
                              ): Set[P] =
    (1 to cyclesToRun).foldLeft(map) { (state, index) =>
      nextPositions(state, index).par.flatMap { pos =>
        if (nextState(state.contains(pos), nextStateValue(pos, state))) Some(pos) else None
      }.seq
    }

  def neighbors[P <: PosTrait[P]](state: Set[P], i: Int): Set[P] =
    state.flatMap(_.neighbors)
}
