package adventofcode.y2015

import scala.collection.mutable

object Day10 extends Year2015 {
  override val day: Int = 10

  private val sameDigitsRE = """(\w)\1*""".r

  override def runDay(input: String): Unit = {
    val part1 = transform(input, 40)
    printDayPart(1, part1.length)
    printDayPart(2, transform(part1, 10).length)
  }

  private def transform(inputStr: String, iterations: Int) = {
    var string = inputStr
    (1 to iterations).foreach { _ =>
      string = transformOnce(string)
    }
    string
  }

  // slow
  private def transformOnceRegex(string: String) =
    sameDigitsRE.replaceAllIn(string, m => {
      m.matched.length.toString + m.matched.charAt(0)
    })

  private def transformOnce(str: String) = {
    val ret = new mutable.StringBuilder
    var currentDigit = str.charAt(0)
    var counter = 0
    str.foreach { d =>
      if (currentDigit == d) {
        counter += 1
      } else {
        ret ++= counter.toString + currentDigit
        currentDigit = d
        counter = 1
      }
    }
    ret ++= counter.toString + currentDigit
    ret.toString()
  }
}
