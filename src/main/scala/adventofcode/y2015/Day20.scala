package adventofcode.y2015

object Day20 extends Year2015 {
  override val day: Int = 20

  val targetNumberOfPresents = inputString.toInt
  val targetPart1 = targetNumberOfPresents / 10
  val targetPart2 = targetNumberOfPresents / 11
  val max = targetNumberOfPresents / 40 // maybe bigger max value?

  var houses = Map.empty[Int, Int].withDefaultValue(0)
  for (elf <- 1 to max;
       house <- elf to max by elf) {
    houses += house -> (houses(house) + elf)
  }
  printDayPart(1, (1 to max).find(houses(_) > targetPart1).get)

  houses = Map.empty[Int, Int].withDefaultValue(0)
  for (elf <- 1 to max;
       house <- (elf to max by elf).take(50)) {
    houses += house -> (houses(house) + elf)
  }
  printDayPart(2, (1 to max).find(houses(_) > targetPart2).get)
}
