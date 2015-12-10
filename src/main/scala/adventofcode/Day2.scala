package adventofcode

object Day2 extends DayApp {
  override val day: Int = 2

  var totalPaper = 0
  var totalRibbon = 0

  input.getLines().foreach { present =>
    val Array(l, w, h) = present.split("x").map(_.toInt).sorted

    totalPaper += (2*l*w + 2*w*h + 2*h*l) + l*w
    totalRibbon += (l + l + w + w) + (l * w * h)
  }

  printDayPart(1, s"total square feet of wrapping paper: $totalPaper")
  printDayPart(2, s"total feet of ribbon: $totalRibbon")
}
