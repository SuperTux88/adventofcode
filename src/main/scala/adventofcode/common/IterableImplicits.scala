package adventofcode.common

implicit class IterableImplicits[A](it: Iterable[A]) {
  def groupCount[K](f: A => K): Map[K, Int] = {
    it.groupMapReduce(f)(_ => 1)(_ + _)
  }
}
