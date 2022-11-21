package adventofcode.common

import scala.annotation.{tailrec, targetName}
import scala.util.{Success, Try}

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

  def rangeWithReverse(from: Int, end: Int, inclusive: Boolean = true): Seq[Int] = {
    val step = if from < end then 1 else -1
    if inclusive then Range.inclusive(from, end, step) else Range(from, end, step)
  }


  /**
    * Sum of increasing numbers: 1 + 2 + 3 + ... + n
    */
  def increasingSum(n: Int): Int = n * (n + 1) / 2

  /**
    * modular inverse
    */
  // from https://rosettacode.org/wiki/Modular_inverse#Scala
  @tailrec
  def modInv(a: Long, m: Long, x: Long = 1, y: Long = 0): Long =
    if (m == 0) x else modInv(m, a % m, y, x - y * (a / m))

  /**
    * chinese remainder theorem
    */
  // from https://rosettacode.org/wiki/Chinese_remainder_theorem#Scala
  def chineseRemainder(n: List[Long], a: List[Long]): Option[Long] = {
    require(n.size == a.size)
    val prod = n.product

    @tailrec
    def iter(n: List[Long], a: List[Long], sm: Long): Long = {
      def mulInv(a: Long, b: Long): Long = {
        if (b == 1) 1
        else {
          val x1 = modInv(a, b)
          if (x1 < 0) x1 + b else x1
        }
      }

      if (n.nonEmpty) {
        val p = prod / n.head

        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      } else sm
    }

    Try {
      iter(n, a, 0) % prod
    } match {
      case Success(v) => Some(v)
      case _ => None
    }
  }

  import math.Integral.Implicits.infixIntegralOps

  implicit class ExtendedIntegral[A: Integral](val n: A) {
    /**
      * positive modulo
      */
    @targetName("positiveModulo")
    def %+(d: A): A = (n + d) % d
  }
}
