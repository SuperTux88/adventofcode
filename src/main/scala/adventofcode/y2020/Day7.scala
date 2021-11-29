package adventofcode.y2020

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day7 extends Year2020 {
  override val day = 7

  private val RuleRE = """(\w+ \w+) bags contain (.*)\.""".r
  private val BagsRE = """(\d+) (\w+ \w+) bags?""".r

  private val myBag = "shiny gold"

  override def runDay(input: BufferedSource): Unit = {
    val rules = input.getLines().map {
      case RuleRE(color, content) => content match {
        case "no other bags" => color -> Map.empty[String, Int]
        case _ =>
          color -> content.split(", ").map {
            case BagsRE(count, bagColor) => bagColor -> count.toInt
          }.toMap
      }
    }.toMap

    val reverseRules = rules.foldLeft(Map.empty[String, List[String]].withDefaultValue(List())) { (reverseMap, rule) =>
      val (color, contents) = rule
      contents.keys.foldLeft(reverseMap) { (currentMap, content) =>
        currentMap.updated(content, color :: currentMap(content))
      }
    }

    printDayPart(1, findContainingColorBags(reverseRules, Seq(myBag)).size, "possible bags outside of my: %s")
    printDayPart(2, countContent(rules, myBag) - 1, "bags inside my bag: %s")
  }

  @tailrec
  private def findContainingColorBags(reverseRules: Map[String, List[String]], searchFor: Seq[String], allOuterBags: Set[String] = Set()): Set[String] = {
    val outerBags = searchFor.flatMap(reverseRules)
    if (outerBags.nonEmpty)
      findContainingColorBags(reverseRules, outerBags, allOuterBags ++ outerBags)
    else
      allOuterBags
  }

  private def countContent(rules: Map[String, Map[String, Int]], color: String): Int = {
    rules(color).map {
      case (color, count) => countContent(rules, color) * count
    }.sum + 1
  }
}
