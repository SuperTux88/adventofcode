package adventofcode.y2015

// import scala.util.parsing.json.JSON

object Day12 extends Year2015 {
  override val day: Int = 12

  val IntRE = """-?\d+""".r
  val InnerObjectRE = """\{[^{}]+\}""".r
  val RedValueRE = """:"red"""".r.unanchored

  val jsonString = inputString

  printDayPart(1, sum(jsonString), "sum of all numbers: %s")
  printDayPart(2, part2, "sum of all numbers: %s")

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

  private def part2 = {
    var filteredJson = jsonString
    while(InnerObjectRE.findAllIn(filteredJson).nonEmpty) {
      filteredJson = sumObjectsAndFilterRed(filteredJson)
    }
    sum(filteredJson)
  }

  private def sum(json: String) = IntRE.findAllIn(json).map(_.toInt).sum

  private def sumObjectsAndFilterRed(json: String) =
    InnerObjectRE.replaceAllIn(json, _.matched match {
      case RedValueRE() => "0"
      case str => sum(str).toString
    })
}
