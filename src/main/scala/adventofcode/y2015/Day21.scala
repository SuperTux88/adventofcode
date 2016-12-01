package adventofcode.y2015

import scala.annotation.tailrec

object Day21 extends Year2015 {
  override val day: Int = 21

  val weapons = Set(
    Item("Dagger",      8, 4, 0),
    Item("Shortsword", 10, 5, 0),
    Item("Warhammer",  25, 6, 0),
    Item("Longsword",  40, 7, 0),
    Item("Greataxe",   74, 8, 0))
  val armors = Set(
    Item("Naked",       0, 0, 0),
    Item("Leather",    13, 0, 1),
    Item("Chainmail",  31, 0, 2),
    Item("Splintmail", 53, 0, 3),
    Item("Bandedmail", 75, 0, 4),
    Item("Platemail", 102, 0, 5))
  val rings = Set(
    Item("No Ring 1",   0, 0, 0),
    Item("No Ring 2",   0, 0, 0),
    Item("Damage +1",  25, 1, 0),
    Item("Damage +2",  50, 2, 0),
    Item("Damage +3", 100, 3, 0),
    Item("Defense +1", 20, 0, 1),
    Item("Defense +2", 40, 0, 2),
    Item("Defense +3", 80, 0, 3))

  val boss = Player.boss(input.getLines().map(_.split(':')(1).trim.toInt))

  val combinations = for {
      weapon <- weapons
      armor <- armors
      ring1 <- rings
      ring2 <- rings - ring1
  } yield Player.player(weapon, armor, ring1, ring2)
  val sortedCombinations = combinations.toSeq.sortBy(_.cost)

  printDayPart(1, sortedCombinations.dropWhile(!simulate(_, boss)).head.cost, "cheapest win: %s")
  printDayPart(2, sortedCombinations.reverse.dropWhile(simulate(_, boss)).head.cost, "most expensive to lose: %s")

  @tailrec
  def simulate(attacker: Player, enemy: Player): Boolean =
    enemy.attackedBy(attacker) match {
      case Some(newEnemy) => simulate(newEnemy, attacker)
      case None => attacker.name == "Player"
    }

  case class Item(name: String, cost: Int, damage: Int, armor: Int)

  case class Player(name: String, hitPoints: Int, damage: Int, armor: Int, cost: Int) {
    def attackedBy(attacker: Player) =
      hitPoints - (attacker.damage - armor).max(1) match {
        case newHitPoints if newHitPoints > 0 => Some(copy(hitPoints = newHitPoints))
        case _ => None
      }
  }
  object Player {
    def player(items: Item*) =
      Player("Player", 100, items.map(_.damage).sum, items.map(_.armor).sum, items.map(_.cost).sum)
    def boss(stats: Iterator[Int]) =
      Player("Boss", stats.next(), stats.next(), stats.next(), 0)
  }
}
