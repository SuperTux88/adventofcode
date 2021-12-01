package adventofcode.y2020

import scala.annotation.tailrec

object MiscFunctions {
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
