package adventofcode.y2023

import scala.collection.immutable.ListMap

object Day15 extends Year2023 {
  override val day = 15

  override def runDay(input: String): Unit = {
    val sequence = input.split(",").toList

    printDayPart(1, sequence.map(hash).sum, "Sum of all hash values: %s")

    val emptyBoxes = (0 to 255).map(_ -> ListMap.empty[String, Int]).toMap
    val boxes = sequence.foldLeft(emptyBoxes) { (boxes, value) =>
      if (value.contains("=")) {
        val Array(label, focalLength) = value.split("=")
        val labelHash = hash(label)
        boxes.updated(labelHash, boxes(labelHash).updated(label, focalLength.toInt))
      } else {
        val label = value.dropRight(1)
        val labelHash = hash(label)
        boxes.updated(labelHash, boxes(labelHash) - label)
      }
    }

    val focusingPower = boxes.flatMap { (box, lenses) =>
      lenses.zipWithIndex.map { case ((_, focalLength), slot) =>
        (box + 1) * (slot + 1) * focalLength
      }
    }

    printDayPart(2, focusingPower.sum, "Focusing power of the resulting lens configuration: %s")
  }

  private def hash(value: String): Int =
    value.foldLeft(0)((current, char) => (current + char.toInt) * 17 % 256)
}
