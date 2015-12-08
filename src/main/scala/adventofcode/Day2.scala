package adventofcode

object Day2 extends App {
  var totalPaper = 0
  var totalRibbon = 0

  Input(2).lines.foreach { present =>
    val Array(l, w, h) = present.split("x").map(_.toInt).sorted

    totalPaper += (2*l*w + 2*w*h + 2*h*l) + l*w
    totalRibbon += (l + l + w + w) + (l * w * h)
  }

  println(s"total square feet of wrapping paper: $totalPaper")
  println(s"total feet of ribbon: $totalRibbon")
}
