package adventofcode.common

object MapImplicits {
  implicit class IntegralMapImplicits[K, V: Integral](map: Map[K, V]) {

    import math.Integral.Implicits.infixIntegralOps

    def changeBy(key: K, diff: V): Map[K, V] = map.updated(key, map(key) + diff)
  }

  implicit class ListMapImplicits[K, V](map: Map[K, List[V]]) {
    def prependWith(key: K, element: V): Map[K, List[V]] = map.updated(key, element :: map(key))
  }
}
