import scala.io.{BufferedSource, Source}

object Input {
  def apply(day: Int) = new Input(Source.fromFile(s"input/day$day.txt"))
}

class Input(file: BufferedSource) {
  def source = file
  def string = file.mkString
  def lines = file.getLines
  def withIndex = file.zipWithIndex
}
