package adventofcode.y2018

object Day3 extends Year2018 {
  override val day = 3

  val ClaimRE = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  val fabric = Array.fill(1000, 1000)(Set[String]())
  var doesNotOverlap = Set[String]()

  input.getLines().foreach {
    case ClaimRE(id, startX, startY, width, height) =>
      doesNotOverlap += id
      for (x <- startX.toInt until startX.toInt + width.toInt;
           y <- startY.toInt until startY.toInt + height.toInt) {
        if (fabric(x)(y).nonEmpty) {
          doesNotOverlap --= fabric(x)(y) + id
        }
        fabric(x)(y) += id
      }
  }

  printDayPart(1, fabric.map(_.count(_.size > 1)).sum)
  printDayPart(2, doesNotOverlap.head.toInt)
}
