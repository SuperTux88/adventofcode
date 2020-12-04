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

  /**
    * number is in range
    */
  def isInRange(n: Long, min: Long, max: Long): Boolean = min <= n && n <= max

  /**
    * modular inverse
    */
  // from https://rosettacode.org/wiki/Modular_inverse#Scala
  @tailrec
  def modInv(a: Long, m: Long, x: Long = 1, y: Long = 0): Long =
    if (m == 0) x else modInv(m, a % m, y, x - y * (a / m))

  implicit class ExtendedLong(val l: Long) {
    /**
      * positive modulo
      */
    def %+(d: Long): Long = (l + d) % d
  }
}
