package adventofcode.y2018

import scala.annotation.tailrec

object Day14 extends Year2018 {
  override val day = 14

  val totalRecipes = inputString.toInt
  val searchedScore = input.map(_.asDigit).toSeq

  val (recipeScores, offset) = calculate(Vector(3,7), 0, 1, Vector.fill(searchedScore.length + 1)(0))

  printDayPart(1, recipeScores.slice(totalRecipes, totalRecipes + 10).mkString.toLong)
  printDayPart(2, recipeScores.length - searchedScore.length - offset)

  @tailrec
  def calculate(recipeScores: Vector[Int], elf1: Int, elf2: Int, tailBuffer: Vector[Int]): (Vector[Int], Int) = {
    if (tailBuffer.take(searchedScore.length) == searchedScore) {
      (recipeScores, 1)
    } else if (tailBuffer.takeRight(searchedScore.length) == searchedScore) {
      (recipeScores, 0)
    } else {
      val (elf1Score, elf2Score) = (recipeScores(elf1), recipeScores(elf2))
      val scoreSum = elf1Score + elf2Score

      val (newScores, newTail) = if (scoreSum < 10) {
                                   (recipeScores :+ scoreSum, tailBuffer.drop(1) :+ scoreSum)
                                 } else {
                                   val newScores = Vector(scoreSum / 10, scoreSum % 10)
                                   (recipeScores ++ newScores, tailBuffer.drop(2) ++ newScores)
                                 }

      calculate(newScores, (elf1 + elf1Score + 1) % newScores.length, (elf2 + elf2Score + 1) % newScores.length, newTail)
    }
  }
}
