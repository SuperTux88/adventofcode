package adventofcode.y2018

import scala.annotation.tailrec

object Day24 extends Year2018 {
  override val day = 24

  private val UnitGroupRE = """(\d+) units each with (\d+) hit points (?:\(([\w,; ]+)\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r
  private val ImmunitiesRE = """immune to ([\w, ]+)""".r
  private val WeaknessesRE = """weak to ([\w, ]+)""".r

  override def runDay(input: String): Unit = {
    val groups = input.split("\n\n").map(_.split("\n").iterator)

    val unitGroups = groups.flatMap { group =>
      val groupType = if (group.next() == "Immune System:") ImmuneSystem else Infection
      group.map {
        case UnitGroupRE(count, hp, weaknessesAndImmunities, attackPower, attackType, initiative) =>
          val (weaknesses, immunities) = parseWeaknessesAndImmunities(weaknessesAndImmunities)
          UnitGroup(groupType, count.toInt, hp.toInt, immunities, weaknesses, attackPower.toInt, attackType, initiative.toInt)
      }
    }.toList

    printDayPart(1, simulate(unitGroups).get._2.map(_.count).sum)
    printDayPart(2, simulateBoost(unitGroups).map(_.count).sum)
  }

  // returns map attacking -> defending
  private def targetSelection(groups: Seq[UnitGroup]): Map[Int, Int] = {
    groups.sortBy(g => (-g.effectivePower, -g.initiative)).foldLeft(Map[Int, Int]()) { (targets, group) =>
      val possibleTargets = groups.filterNot(t =>
        t.groupType == group.groupType || targets.contains(t.initiative) || t.immunities.contains(group.attackType))
      if (possibleTargets.nonEmpty) {
        val target = possibleTargets.maxBy(t => (group.damageTo(t), t.effectivePower, t.initiative))
        targets + (target.initiative -> group.initiative)
      } else targets
    }.map(_.swap)
  }

  private def attack(groups: Seq[UnitGroup], targets: Map[Int, Int]): Seq[UnitGroup] = {
    val groupMap = groups.map { g => g.initiative -> g }.toMap
    groups.map(_.initiative).sortBy(-_).foldLeft(groupMap) { (remaining, attackingId) =>
      if (remaining.contains(attackingId) && targets.contains(attackingId)) {
        val target = remaining(targets(attackingId))
        val targetAfterAttack = target.takeHit(remaining(attackingId).damageTo(target))
        if (targetAfterAttack.count > 0)
          remaining + (target.initiative -> targetAfterAttack)
        else
          remaining - target.initiative
      } else remaining
    }.values.toList
  }

  @tailrec
  private def simulate(groups: Seq[UnitGroup]): Option[(GroupType, List[UnitGroup])] = {
    if (groups.groupBy(_.groupType).size == 1) {
      Some(groups.head.groupType, groups.toList)
    } else {
      val newUnits = attack(groups, targetSelection(groups))
      if (newUnits == groups) None
      else simulate(newUnits)
    }
  }

  @tailrec
  private def simulateBoost(groups: Seq[UnitGroup], boost: Int = 1): List[UnitGroup] = {
    val boosted = groups.map {
      case group if group.groupType == ImmuneSystem => group.boost(boost)
      case group => group
    }
    simulate(boosted) match {
      case Some((ImmuneSystem, left)) => left
      case _ => simulateBoost(groups, boost + 1)
    }
  }

  private def parseWeaknessesAndImmunities(str: String) = {
    var weaknesses, immunities = Set[String]()
    if (str != null) {
      str.split("; ").foreach {
        case ImmunitiesRE(s) => immunities = s.split(", ").toSet
        case WeaknessesRE(s) => weaknesses = s.split(", ").toSet
      }
    }
    (weaknesses, immunities)
  }

  sealed trait GroupType
  private case object ImmuneSystem extends GroupType
  private case object Infection extends GroupType

  private case class UnitGroup(groupType: GroupType, count: Int, hp: Int,
                               immunities: Set[String], weaknesses: Set[String],
                               attackPower: Int, attackType: String, initiative: Int) {
    def takeHit(attack: Int): UnitGroup = copy(count = count - attack/hp)
    def boost(boost: Int): UnitGroup = copy(attackPower = attackPower + boost)

    def effectivePower: Int = count * attackPower
    def damageTo(group: UnitGroup): Int = {
      if (group.immunities.contains(attackType))
        0
      else if (group.weaknesses.contains(attackType))
        effectivePower * 2
      else
        effectivePower
    }
  }
}
