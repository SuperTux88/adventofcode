package adventofcode.y2019

import adventofcode.DayApp

trait Year2019 extends DayApp {
  override val year: Int = 2019

  override def runDay(input: String): Unit = {
    runDay(new IntCode(input))
  }

  def runDay(input: IntCode): Unit = ???
}
