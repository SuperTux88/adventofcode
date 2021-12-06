package adventofcode.common

implicit class IterableImplicits[A](it: Iterable[A]) {
  def groupCount[K](f: A => K): Map[K, Long] = {
    it.groupMapReduce(f)(_ => 1L)(_ + _)
  }
}
