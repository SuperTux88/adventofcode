package adventofcode.common

object MapImplicits {
  implicit class MapImplicits[K, V](map: Map[K, V]) {
    def findKeyByValue(value: V): Option[K] = map.find(_._2 == value).map(_._1)
    def keySetByValue(value: V): Set[K] = map.filter(_._2 == value).keySet
  }

  implicit class IntegralMapImplicits[K, V: Integral](map: Map[K, V]) {

    import math.Integral.Implicits.infixIntegralOps

    def changeBy(key: K, diff: V): Map[K, V] = map.updatedWith(key)(_.map(_ + diff).orElse(Some(diff)))
  }

  implicit class ListMapImplicits[K, V](map: Map[K, List[V]]) {
    def prependWith(key: K, element: V): Map[K, List[V]] = map.updated(key, element :: map(key))
  }
}
