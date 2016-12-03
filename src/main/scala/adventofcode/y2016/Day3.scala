package adventofcode.y2016

object Day3 extends Year2016 {
  override val day: Int = 3

  val numbers = input.getLines().map(_.trim.split(" +").map(_.toInt)).toList

  printDayPart(1, numbers.count(isTriangle))

  val numbers2 = numbers.sliding(3, 3).flatMap { numberGroup =>
    (0 to 2).map(group => Array(numberGroup.head(group), numberGroup(1)(group), numberGroup(2)(group)))
  }

  printDayPart(2, numbers2.count(isTriangle))

  private def isTriangle(sides: Array[Int]) = {
    val (a, b, c) = (sides(0), sides(1), sides(2))
    a+b>c && a+c>b && b+c>a
  }
}
