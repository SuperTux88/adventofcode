package adventofcode.common

import scala.annotation.tailrec

object NumberHelper {

  /**
    * greatest common divisor
    */
  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)

  /**
    * least common multiple
    */
  def lcm(a: Long, b: Long): Long = a * (b / gcd(a, b))
  def lcm(numbers: Seq[Long]): Long = numbers.reduce(lcm)
}
