package adventofcode

object AllDays {
  def allYears: List[DayApp] = (2015 to 2020).flatMap(year).toList

  def year(year: Int): List[DayApp] =
      (1 to 25).map(dayObj(year, _)).takeWhile(_.isDefined).flatten.toList

  def yearExists(year: Int): Boolean = try {
    Class.forName(s"adventofcode.y$year.Year$year")
    true
  } catch {
    case _: ClassNotFoundException => false
  }

  private def dayObj(year: Int, day: Int) =
    try {
      val c = Class.forName(s"adventofcode.y$year.Day$day$$")
      Some(c.getField("MODULE$").get(c).asInstanceOf[DayApp])
    } catch {
      case _: ClassNotFoundException => None
    }
}
