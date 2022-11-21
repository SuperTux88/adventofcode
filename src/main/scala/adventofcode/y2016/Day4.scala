package adventofcode.y2016

import scala.io.BufferedSource

object Day4 extends Year2016 {
  override val day: Int = 4

  private val RoomRE = """([\w-]+)-(\d+)\[(\w{5})]""".r

  override def runDay(input: BufferedSource): Unit = {
    val realRooms = input.getLines().map {
      case RoomRE(name, sectorId, checksum) => Room(name, sectorId.toInt, checksum)
    }.filter(_.isReal).toList

    printDayPart(1, realRooms.map(_.sectorId).sum)
    printDayPart(2, realRooms.find(_.decrypt == "northpole object storage").get.sectorId)
  }

  private case class Room(name: String, sectorId: Int, checksum: String) {
    def isReal: Boolean = {
      val calculatedChecksum = name.filter(_ != '-').toSeq.groupBy(identity).toSeq
        .sortBy(_._1).sortBy(-_._2.length).take(5).map(_._1).mkString
      calculatedChecksum == checksum
    }

    def decrypt: String = {
      val key = sectorId % 26
      name.map {
        case '-' => ' '
        case c if c + key <= 'z' => (c + key).toChar
        case c if c + key  > 'z' => (c + key - 26).toChar
      }.mkString
    }
  }
}
