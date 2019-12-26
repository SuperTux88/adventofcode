package adventofcode.common.search

import scala.collection.mutable

object AStar {
  /**
    * @param start     start point
    * @param isTarget  target point
    * @param neighbors get new neighbors: (step-cost, heuristic, neighbor)
    * @tparam T        point type
    * @return          (total cost, path)
    */
  def apply[T](start: T, isTarget: T => Boolean, neighbors: T => List[(Int, Int, T)]): (Int, List[T]) = {
    val Q = mutable.PriorityQueue((0, 0, List(start)))(Ordering.by(-_._2)) // order by heuristic
    val seen = mutable.Set[T]()

    while(Q.nonEmpty) {
      val (cost, _, current) = Q.dequeue()
      if (isTarget(current.head)) {
        return (cost, current)
      } else if (seen.add(current.head)) {
        neighbors(current.head).foreach { n =>
          val nCost = cost + n._1
          Q.enqueue((nCost, nCost + n._2, n._3 :: current))
        }
      }
    }

    (0, Nil) // no path found
  }
}
