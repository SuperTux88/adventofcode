package adventofcode.y2018

import adventofcode.Logging
import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day15 extends Year2018 {
  override val day = 15

  override def runDay(input: BufferedSource): Unit = {
    val parsedInput = input.getLines().zipWithIndex.map {
      case (line, y) =>
        line.zipWithIndex.map {
          case (symbol@('E' | 'G'), x) => ('.', Some(Enemy(Pos(x, y), symbol)))
          case (symbol, _) => (symbol, None)
        }
    }.toList

    val (map, enemies) = (Grid(parsedInput.map(_.map(_._1))), parsedInput.flatMap(_.flatMap(_._2)))

    val (survivors, rounds) = simulate(3, map, enemies)

    map.print(survivors)

    printDayPart(1, survivors.map(_.hp).sum * rounds)
    printDayPart(2, simulateWin(4, map, enemies))
  }

  @tailrec
  private def simulate(elfAttackPower: Int, map: Grid, players: List[Enemy], playersDone: List[Enemy] = Nil, round: Int = 1): (List[Enemy], Int) = {
    players match {
      case Nil if playersDone.groupBy(_.symbol).size == 1 => (playersDone, round)
      case Nil => simulate(elfAttackPower, map, playersDone.sortBy(_.pos), Nil, round + 1)
      case currentPlayer :: playersTodo =>
        val otherPlayers: List[Enemy] = playersTodo ::: playersDone
        val targets = otherPlayers.filter(_.symbol != currentPlayer.symbol)

        if (targets.isEmpty) {
          (currentPlayer :: otherPlayers, round - 1)
        } else {
          var newPos = currentPlayer.pos
          val freeEnemyPositions = targets.flatMap(_.freeNeighbors(map, otherPlayers))

          if (!freeEnemyPositions.contains(newPos)) {
            val newPositions = findNearestPosition(map, otherPlayers, currentPlayer.pos, freeEnemyPositions)
            if (newPositions.nonEmpty) {
              val possibleDirections = currentPlayer.pos.directions.filter(pos => map.isFree(otherPlayers, pos))
              val nearestDirection = findNearestPosition(map, otherPlayers, newPositions.min, possibleDirections)
              if (nearestDirection.nonEmpty) newPos = nearestDirection.min
            }
          }

          val possibleTargets = newPos.directions.flatMap { pos =>
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

              simulate(elfAttackPower, map, newTodo, currentPlayer.copy(pos = newPos) :: newDone, round)
          }
        }
    }
  }

  @tailrec
  private def simulateWin(elfAttackPower: Int, map: Grid, players: List[Enemy]): Int = {
    printDebug(s"Trying elf attack power: $elfAttackPower")
    val elves = players.count(_.symbol == 'E')

    val (survivors, rounds) = simulate(elfAttackPower, map, players)
    map.print(survivors)

    if (survivors.count(_.symbol == 'E') == elves) {
      printDebug(s"Elves win without a single death with attack power $elfAttackPower! \\o/")
      survivors.map(_.hp).sum * rounds
    } else {
      printDebug(s"Elf died after $rounds rounds :(")
      simulateWin(elfAttackPower + 1, map, players)
    }
  }

  private def patchPlayers(players: List[Enemy], target: Option[Enemy], replacement: Option[Enemy]) =
    target match {
      case Some(enemy) =>
        players.flatMap(p => if (p == enemy) replacement else Some(p))
      case None => players
    }

  private def findNearestPosition(map: Grid, players: Seq[Enemy], pos: Pos, targets: Seq[Pos]): Seq[Pos] = {
    @tailrec
    def step(visited: List[Pos], current: List[Pos]): List[Pos] = {
      val nextSteps = current.flatMap { pos =>
        pos.directions.filter(pos => map.isFree(players, pos) && !visited.contains(pos))
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

    def freeNeighbors(map: Grid, players: Seq[Enemy]): Seq[Pos] =
      pos.directions.filter(map.isFree(players, _))

    def stats: String = s"$symbol($hp)"
  }

  private case class Grid(grid: Seq[Seq[Char]]) {
    def isFree(players: Seq[Enemy], pos: Pos): Boolean =
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
