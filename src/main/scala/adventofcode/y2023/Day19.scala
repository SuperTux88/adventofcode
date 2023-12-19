package adventofcode.y2023

import scala.annotation.tailrec

object Day19 extends Year2023 {
  override val day = 19

  private val RulesRE = """(\w+)\{(.*)}""".r
  private val RuleRE = """([xmas])([<>])(\d+):(\w+)""".r

  private val VARIABLE_INDEXES = "xmas"

  override def runDay(input: String): Unit = {
    val (workflowsStr, partsStr) = input.splitAt(input.indexOf("\n\n"))
    val workflows = workflowsStr.split("\n").map {
      case RulesRE(name, rulesStr) =>
        val rulesStrSplit = rulesStr.split(",")
        val rules = rulesStrSplit.init.map {
          case RuleRE(variable, condition, number, target) => Rule(VARIABLE_INDEXES.indexOf(variable.head), condition.head, number.toInt, target)
        }.toList
        name -> Workflow(rules, rulesStrSplit.last)
    }.toMap

    val parts = partsStr.trim.split("\n").map { partStr =>
      partStr.tail.init.split(",").map { valueStr =>
        val Array(_, value) = valueStr.split("=")
        value.toInt
      }.toList
    }

    val accepted = parts.flatMap(validatePart(workflows, _))
    printDayPart(1, accepted.map(_.sum).sum, "Sum of ratings of accepted parts: %s")

    val ranges = splitWorkflowsIntoRanges(workflows)
    val combinations = ranges.map { range =>
      range.map { case (min, max) => (max - min + 1).toLong }.product
    }.sum
    printDayPart(2, combinations, "Distinct combinations of ratings for accepted parts: %s")
  }

  @tailrec
  private def validatePart(workflows: Map[String, Workflow], part: List[Int], workflow: String = "in"): Option[List[Int]] =
    workflows(workflow).getTarget(part) match {
      case "A" => Some(part)
      case "R" => None
      case nextWorkflow => validatePart(workflows, part, nextWorkflow)
    }

  private def splitWorkflowsIntoRanges(workflows: Map[String, Workflow]): List[List[(Int, Int)]] = {
    def updateRanges(variable: Int, condition: Char, value: Int, ranges: List[List[(Int, Int)]]) =
      ranges.map { range =>
        val (min, max) = range(variable)
        condition match {
          case '<' =>
            if (value <= max) range.updated(variable, (min, value - 1))
            else range
          case '>' =>
            if (value >= min) range.updated(variable, (value + 1, max))
            else range
        }
      }

    def targetRange(target: String) = target match {
      case "A" => List(List.fill(4)(1, 4000))
      case "R" => Nil
      case nextWorkflow => inner(workflows(nextWorkflow))
    }

    def inner(workflow: Workflow): List[List[(Int, Int)]] = workflow match {
      case Workflow(Nil, target) => targetRange(target)
      case Workflow(Rule(variable, condition, number, target) :: rules, catchAll) =>
        val matchRule = updateRanges(variable, condition, number, targetRange(target))
        val matchNextRule = condition match {
          case '<' => updateRanges(variable, '>', number - 1, inner(Workflow(rules, catchAll)))
          case '>' => updateRanges(variable, '<', number + 1, inner(Workflow(rules, catchAll)))
        }
        matchRule ::: matchNextRule
    }

    inner(workflows("in"))
  }

  private case class Workflow(rules: List[Rule], catchAll: String) {
    def getTarget(part: List[Int]): String = rules.find(_.matches(part)).map(_.target).getOrElse(catchAll)
  }

  private case class Rule(variable: Int, condition: Char, number: Int, target: String) {
    def matches(part: List[Int]): Boolean = condition match {
      case '<' => part(variable) < number
      case '>' => part(variable) > number
    }
  }
}
