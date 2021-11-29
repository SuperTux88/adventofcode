package adventofcode.y2015

import scala.io.BufferedSource

object Day16 extends Year2015 {
  override val day: Int = 16

  private val SueRE = """Sue (\d+):(.*)""".r
  private val CompoundRE = """ (\w+): (\d+)""".r

  private val searchedSueCompounds = Map(
    "children" -> 3,
    "cats" -> 7,
    "samoyeds" -> 2,
    "pomeranians" -> 3,
    "akitas" -> 0,
    "vizslas" -> 0,
    "goldfish" -> 5,
    "trees" -> 3,
    "cars" -> 2,
    "perfumes" -> 1
  )

  override def runDay(input: BufferedSource): Unit = {
    val sues = input.getLines().map {
      case SueRE(number, compounds) =>
        Sue(number.toInt, compounds.split(',').map {
          case CompoundRE(thing, value) => thing -> value.toInt
        }.toMap)
    }.toList

    printDayPart(1, sues.find(_.matchesCompounds(searchedSueCompounds)).get.number)
    printDayPart(2, sues.find(_.isRealSue(searchedSueCompounds)).get.number)
  }

  private case class Sue(number: Int, compounds: Map[String, Int]) {
    def matchesCompounds(searchedCompounds: Map[String, Int]): Boolean =
      !compounds.exists(c => searchedCompounds(c._1) != c._2)

    def isRealSue(searchedCompounds: Map[String, Int]): Boolean =
      !compounds.exists(c => c._1 match {
        case "cats" | "trees" => searchedCompounds(c._1) >= c._2
        case "pomeranians" | "goldfish" => searchedCompounds(c._1) <= c._2
        case _ => searchedCompounds(c._1) != c._2
      })
  }
}
