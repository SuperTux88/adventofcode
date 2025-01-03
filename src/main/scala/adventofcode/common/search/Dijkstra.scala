package adventofcode.common.search

import scala.collection.mutable

object Dijkstra {
  /**
    * @param start     start point
    * @param isTarget  target point
    * @param neighbors get new neighbors: (step-cost, neighbor)
    * @tparam T        point type
    * @return          (total cost, path)
    */
  def apply[T](start: T, isTarget: T => Boolean, neighbors: T => List[(Int, T)]): (Int, List[T]) = {
    val Q = mutable.PriorityQueue((0, List(start)))(Ordering.by(-_._1)) // order by cost
    val seen = mutable.Set[T]()

    while (Q.nonEmpty) {
      val (cost, current) = Q.dequeue()
      if (isTarget(current.head)) {
        return (cost, current)
      } else if (seen.add(current.head)) {
        neighbors(current.head).foreach { (nextCost, nextState) =>
          Q.enqueue((cost + nextCost, nextState :: current))
        }
      }
    }

    (0, Nil) // no path found
  }
}
