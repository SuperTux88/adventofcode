package adventofcode.y2016

object Day3 extends Year2016 {
  override val day: Int = 3

  val numbers = input.getLines.map(_.trim.split(" +").map(_.toInt).toList).toList

  printDayPart(1, numbers.count(isTriangle))
  printDayPart(2, numbers.grouped(3).flatMap(_.transpose).count(isTriangle))

  private def isTriangle(sides: List[Int]) = {
    val (a, b, c) = (sides.head, sides(1), sides(2))
    a+b>c && a+c>b && b+c>a
  }
}
