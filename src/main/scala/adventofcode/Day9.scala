package adventofcode

object Day9 extends DayApp {
  override val day: Int = 9

  val DistanceRE = """(\w+) to (\w+) = (\d+)""".r

  val distances = input.getLines().map {
    case DistanceRE(a, b, distance) => (a, b) -> distance.toInt
  }.toMap

  val routes = distances.flatMap {
    case ((a, b), _) => Seq(a, b)
  }.toSeq.distinct.permutations

  val routeResults = routes.map { route =>
    route.sliding(2).map {
      case Seq(a, b) => distances.getOrElse((a, b), distances(b, a))
    }.sum
  }.toSeq

  printDayPart(1, routeResults.min, "shortest route: %s")
  printDayPart(2, routeResults.max, "longest route: %s")
}
