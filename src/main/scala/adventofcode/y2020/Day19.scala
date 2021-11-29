package adventofcode.y2020

import scala.io.BufferedSource

object Day19 extends Year2020 {
  override val day = 19

  private val RuleRE = """(\d+): (.*)""".r
  private val CharRE = """\"(\w)\"""".r

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val rules = lines.takeWhile(_.nonEmpty).map {
      case RuleRE(num, rule) => rule match {
        case CharRE(char) =>
          num.toInt -> CharRule(char)
        case subRules =>
          num.toInt -> SubRules(subRules.split(" \\| ").map(_.split(" ").map(_.toInt).toList).toList)
      }
    }.toMap
    val messages = lines.toList

    printDayPart(1, messages.count(isValid(_, rules)), "valid messages: %s")

    val updatesRules = rules
      .updated(8, SubRules(List(List(42), List(42, 8))))
      .updated(11, SubRules(List(List(42, 31), List(42, 11, 31))))

    printDayPart(2, messages.count(isValid(_, updatesRules)), "valid messages with updated rules: %s")
  }

  private def isValid(message: String, rules: Map[Int, Rule], ruleNumbersToCheck: List[Int] = List(0)): Boolean =
    if (ruleNumbersToCheck.isEmpty) {
      message.isEmpty
    } else {
      val currentRuleNumbers :: remainingRuleNumbers = ruleNumbersToCheck
      rules(currentRuleNumbers) match {
        case CharRule(char) =>
          message.startsWith(char) && isValid(message.drop(1), rules, remainingRuleNumbers)
        case SubRules(ruleSets) =>
          ruleSets.exists(ruleSet => isValid(message, rules, ruleSet ::: remainingRuleNumbers))
      }
    }

  private sealed trait Rule
  private case class CharRule(char: String) extends Rule
  private case class SubRules(ruleSets: List[List[Int]]) extends Rule
}
