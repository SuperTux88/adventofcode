package adventofcode.y2018

object Day8 extends Year2018 {
  override val day = 8

  override def runDay(input: String): Unit = {
    val (metadataSum, totalValue, _) = calculateNode(input.split(" ").map(_.toInt).toList)

    printDayPart(1, metadataSum)
    printDayPart(2, totalValue)
  }

  private def calculateNode(numbers: List[Int]): (Int, Int, List[Int]) = {
    val childrenCount :: metadataCount :: body = numbers: @unchecked

    var remainingNumbers = body

    val children = (0 until childrenCount).map { _ =>
      val (metadataSum, nodeValue, otherNodesNumbers) = calculateNode(remainingNumbers)
      remainingNumbers = otherNodesNumbers
      (metadataSum, nodeValue)
    }

    val (metadata, otherNodesNumbers) = remainingNumbers.splitAt(metadataCount)

    val nodeValue = if (childrenCount == 0) {
                      metadata.sum
                    } else {
                      metadata.filter(_ <= children.size).map(m => children(m - 1)._2).sum
                    }

    (children.map(_._1).sum + metadata.sum, nodeValue, otherNodesNumbers)
  }
}
