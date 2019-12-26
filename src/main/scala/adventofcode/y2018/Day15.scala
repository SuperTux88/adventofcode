package adventofcode.y2018

import adventofcode.Logging
import adventofcode.common.pos.Pos

import scala.annotation.tailrec

object Day15 extends Year2018 {
  override val day = 15

  private val parsedInput = input.getLines().zipWithIndex.map {
    case (line, y) =>
      line.zipWithIndex.map {
        case (symbol@('E'|'G'), x) => ('.',    Some(Enemy(Pos(x, y), symbol)))
        case (symbol, _)           => (symbol, None)
      }
  }.toList

  private val (map, enemies) = (Grid(parsedInput.map(_.map(_._1))), parsedInput.flatMap(_.flatMap(_._2)))

  private val (survivors, rounds) = simulate(3, enemies)

  map.print(survivors)

  printDayPart(1, survivors.map(_.hp).sum * rounds)
  printDayPart(2, simulateWin(4, enemies))

  @tailrec
  private def simulate(elfAttackPower: Int, players: List[Enemy], playersDone: List[Enemy] = Nil, round: Int = 1): (List[Enemy], Int) = {
    players match {
      case Nil if playersDone.groupBy(_.symbol).size == 1 => (playersDone, round)
      case Nil => simulate(elfAttackPower, playersDone.sortBy(_.pos), Nil, round + 1)
      case currentPlayer :: playersTodo =>
        implicit val otherPlayers: List[Enemy] = playersTodo ::: playersDone
        val targets = otherPlayers.filter(_.symbol != currentPlayer.symbol)

        if (targets.isEmpty) {
          (currentPlayer :: otherPlayers, round - 1)
        } else {
          var newPos = currentPlayer.pos
          val freeEnemyPositions = targets.flatMap(_.freeNeighbors)

          if (!freeEnemyPositions.contains(newPos)) {
            val newPositions = findNearestPosition(currentPlayer.pos, freeEnemyPositions)
            if (newPositions.nonEmpty) {
              val possibleDirections = Pos.directions.map(currentPlayer.pos + _).filter(pos => map.isFree(pos))
              val nearestDirection = findNearestPosition(newPositions.min, possibleDirections)
              if (nearestDirection.nonEmpty) newPos = nearestDirection.min
            }
          }

          val possibleTargets = Pos.directions.map(newPos + _).flatMap { pos =>
            targets.find(_.pos == pos)
          }

          val targetOption = if (possibleTargets.nonEmpty) {
            Some(possibleTargets.groupBy(t => t.hp).minBy(_._1)._2.minBy(_.pos))
          } else None

          targetOption match {
            case Some(Enemy(_, 'E', hp)) if elfAttackPower > 3 && hp <= 3 =>
              (patchPlayers(players ::: playersDone, targetOption, None), round - (if (playersTodo.isEmpty) 0 else 1))
            case _ =>
              val targetAfterHit = targetOption.flatMap(_.takeHit(if (currentPlayer.symbol == 'E') elfAttackPower else 3))

              val newTodo = patchPlayers(playersTodo, targetOption, targetAfterHit)
              val newDone = patchPlayers(playersDone, targetOption, targetAfterHit)

              simulate(elfAttackPower, newTodo, currentPlayer.copy(pos = newPos) :: newDone, round)
          }
        }
    }
  }

  @tailrec
  def simulateWin(elfAttackPower: Int, players: List[Enemy]): Int = {
    printDebug(s"Trying elf attack power: $elfAttackPower")
    val elves = players.count(_.symbol == 'E')

    val (survivors, rounds) = simulate(elfAttackPower, players)
    map.print(survivors)

    if (survivors.count(_.symbol == 'E') == elves) {
      printDebug(s"Elves win without a single death with attack power $elfAttackPower! \\o/")
      survivors.map(_.hp).sum * rounds
    } else {
      printDebug(s"Elf died after $rounds rounds :(")
      simulateWin(elfAttackPower + 1, players)
    }
  }

  private def patchPlayers(players: List[Enemy], target: Option[Enemy], replacement: Option[Enemy]) =
    target match {
      case Some(enemy) =>
        players.flatMap(p => if (p == enemy) replacement else Some(p))
      case None => players
    }

  def findNearestPosition(pos: Pos, targets: Seq[Pos])(implicit players: Seq[Enemy]): Seq[Pos] = {
    @tailrec
    def step(visited: List[Pos], current: List[Pos]): List[Pos] = {
      val nextSteps = current.flatMap { pos =>
        Pos.directions.map(pos + _).filter(pos => map.isFree(pos) && !visited.contains(pos))
      }.distinct

      val found = nextSteps.intersect(targets)
      if (found.nonEmpty || nextSteps.isEmpty)
        found
      else
        step(visited ::: nextSteps, nextSteps)
    }

    if (targets.contains(pos)) List(pos) else step(List(pos), List(pos))
  }

  private case class Enemy(pos: Pos, symbol: Char, hp: Int = 200) {
    def takeHit(attackPower: Int): Option[Enemy] =
      if (hp > attackPower) Some(copy(hp = hp - attackPower)) else None

    def freeNeighbors(implicit players: Seq[Enemy]): Seq[Pos] =
      Pos.directions.map(pos + _).filter(map.isFree(_))

    def stats: String = s"$symbol($hp)"
  }

  private case class Grid(grid: Seq[Seq[Char]]) {
    def isFree(pos: Pos)(implicit players: Seq[Enemy]): Boolean =
      grid(pos.y)(pos.x) != '#' && !players.exists(_.pos == pos)

    def print(enemies: Seq[Enemy]): Unit = if (Logging.debug)
      grid.zipWithIndex.foreach {
        case (line, y) =>
          val lineEnemies = enemies.filter(_.pos.y == y)
          val mapLine = line.zipWithIndex.map {
            case (symbol, x) =>
              val enemy = lineEnemies.find(_.pos == Pos(x, y))
              if (enemy.isEmpty) symbol else enemy.get.symbol
          }.mkString
          println(s"$mapLine    ${lineEnemies.map(_.stats).mkString(", ")}".trim)
      }
  }
}
