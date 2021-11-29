package adventofcode.y2015

// import scala.util.parsing.json.JSON

object Day12 extends Year2015 {
  override val day: Int = 12

  private val IntRE = """-?\d+""".r
  private val InnerObjectRE = """\{[^{}]+\}""".r
  private val RedValueRE = """:"red"""".r.unanchored

  override def runDay(json: String): Unit = {
    printDayPart(1, sum(json), "sum of all numbers: %s")
    printDayPart(2, part2(json), "sum of all numbers: %s")
  }

  private def sum(json: String) = IntRE.findAllIn(json).map(_.toInt).sum

  private def part2(json: String) = {
    var filteredJson = json
    while (InnerObjectRE.findAllIn(filteredJson).nonEmpty) {
      filteredJson = sumObjectsAndFilterRed(filteredJson)
    }
    sum(filteredJson)
  }

  private def sumObjectsAndFilterRed(json: String) =
    InnerObjectRE.replaceAllIn(json, _.matched match {
      case RedValueRE() => "0"
      case str => sum(str).toString
    })

  // slow
  //  private def part2WithJsonParse = {
  //    def sum(any: Any): Int = any match {
  //      case number: Double => number.toInt
  //      case array: List[Any] => array.map(sum).sum
  //      case obj: Map[String, Any] if !obj.values.toList.contains("red") => obj.values.map(sum).sum
  //      case x => 0
  //    }
  //
  //    sum(JSON.parseFull(jsonString).get)
  //  }
}
