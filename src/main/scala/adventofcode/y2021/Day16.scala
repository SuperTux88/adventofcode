package adventofcode.y2021

object Day16 extends Year2021 {
  override val day = 16

  override def runDay(input: String): Unit = {
    val bits = input.flatMap(hexToBin).toList
    val transmission = parsePacket(bits)._1

    printDayPart(1, transmission.versionSum, "sum of all versions: %s")
    printDayPart(2, transmission.value, "result of evaluated transmission: %s")
  }

  private def parsePacket(bits: List[Int]): (Packet, List[Int]) = {
    val (versionBits, typeIdAndPayloadBits) = bits.splitAt(3)
    val (typeIdBits, payloadBits) = typeIdAndPayloadBits.splitAt(3)
    val version = Integer.parseInt(versionBits.mkString, 2)
    Integer.parseInt(typeIdBits.mkString, 2) match {
      case 4 =>
        val (value, remaining) = parseLiteralValue(payloadBits)
        (Literal(version, value), remaining)
      case typeId => (payloadBits: @unchecked) match {
        case 0 :: lengthAndOperatorBits =>
          val (lengthBits, operatorPayloadBits) = lengthAndOperatorBits.splitAt(15)
          val length = Integer.parseInt(lengthBits.mkString, 2)
          val (subpacketBits, remaining) = operatorPayloadBits.splitAt(length)
          (Operator(version, typeId, parseSubPackets(subpacketBits)), remaining)
        case 1 :: lengthAndOperatorBits =>
          val (lengthBits, operatorPayloadBits) = lengthAndOperatorBits.splitAt(11)
          val count = Integer.parseInt(lengthBits.mkString, 2)
          val (subpackets, remaining) = parseSubPackets(operatorPayloadBits, count)
          (Operator(version, typeId, subpackets), remaining)
      }
    }
  }

  private def parseLiteralValue(bits: List[Int]): (Long, List[Int]) = {
    val bitsCount = bits.grouped(5).takeWhile(_.head == 1).size * 5 + 5
    val (groups, rest) = bits.splitAt(bitsCount)
    (java.lang.Long.parseLong(groups.grouped(5).flatMap(_.drop(1)).mkString, 2), rest)
  }

  private def parseSubPackets(bits: List[Int]): Vector[Packet] =
    parsePacket(bits) match {
      case (packet, Nil) => Vector(packet)
      case (packet, remaining) => packet +: parseSubPackets(remaining)
    }

  private def parseSubPackets(bits: List[Int], count: Int): (Vector[Packet], List[Int]) =
    (0 until count).foldLeft(Vector[Packet](), bits) {
      case ((packets, bits), _) =>
        val (packet, remaining) = parsePacket(bits)
        (packets :+ packet, remaining)
    }

  private def hexToBin(char: Char): List[Int] =
    f"${Integer.parseInt(char.toString, 16).toBinaryString.toInt}%04d".map(_.asDigit).toList

  private sealed trait Packet {
    val version: Int
    def versionSum: Int = version
    def value: Long
  }
  private case class Literal(version: Int, value: Long) extends Packet
  private case class Operator(version: Int, typeId: Int, subPackets: Vector[Packet]) extends Packet {
    override def versionSum: Int = super.versionSum + subPackets.map(_.versionSum).sum
    override def value: Long =
      ((typeId, subPackets.map(_.value)): @unchecked) match {
        case (0, values) => values.sum
        case (1, values) => values.product
        case (2, values) => values.min
        case (3, values) => values.max
        case (5, Seq(val1, val2)) => if val1 > val2 then 1 else 0
        case (6, Seq(val1, val2)) => if val1 < val2 then 1 else 0
        case (7, Seq(val1, val2)) => if val1 == val2 then 1 else 0
      }
  }
}
