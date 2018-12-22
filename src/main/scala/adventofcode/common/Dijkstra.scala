package adventofcode.common

import scala.collection.mutable

object Dijkstra {
  def apply[T](start: T, target: T, neighbors: T => List[(Int, T)]): (Int, List[T]) = {
    val Q = mutable.PriorityQueue((0, List(start)))(Ordering.by(-_._1))
    val seen = mutable.Set[T]()

    while(Q.nonEmpty) {
      val (cost, current) = Q.dequeue()
      if (current.head == target) {
        return (cost, current)
      } else if (seen.add(current.head)) {
        neighbors(current.head).foreach { n =>
          Q.enqueue((cost + n._1, n._2 :: current))
        }
      }
    }

    (0, Nil) // no path found
  }
}
