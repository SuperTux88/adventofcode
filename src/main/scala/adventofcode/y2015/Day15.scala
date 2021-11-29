package adventofcode.y2015

import scala.io.BufferedSource

object Day15 extends Year2015 {
  override val day: Int = 15

  private val IngredientRE = """([\w]+): capacity ([-\d]+), durability ([-\d]+), flavor ([-\d]+), texture ([-\d]+), calories ([-\d]+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val ingredients = input.getLines().map {
      case IngredientRE(n, capacity, durability, flavor, texture, calories) =>
        Ingredient(n, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
    }.toList

    val possibilities = List.fill(ingredients.size)(0 to 100).flatten.combinations(ingredients.size)
      .filter(_.sum == 100).flatMap(_.permutations).map(_.zip(ingredients)).toList

    printDayPart(1, calculateBestResultForPossibilities(possibilities))

    val possibilitiesWith500Calories = possibilities.filter(p => p.map(i => i._2.calories * i._1).sum == 500)
    printDayPart(2, calculateBestResultForPossibilities(possibilitiesWith500Calories))
  }

  private def combinationResult(combination: List[(Int, Ingredient)]) = {
    val scores = combination.map(i => i._2.scoresFor(i._1))
    scores.transpose.map(_.sum.max(0)).product
  }

  private def calculateBestResultForPossibilities(possibilities: List[List[(Int, Ingredient)]]) = {
    possibilities.map(combinationResult).max
  }

  private case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int) {
    def scoresFor(teaspoons: Int): Seq[Int] = Seq(capacity, durability, flavor, texture).map(_*teaspoons)
  }
}
