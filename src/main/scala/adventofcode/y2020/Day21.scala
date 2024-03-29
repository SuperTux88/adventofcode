package adventofcode.y2020

import MiscFunctions.reduceToUniqueValues

import scala.io.BufferedSource

object Day21 extends Year2020 {
  override val day = 21

  private val FoodRE = """([\w ]+) \(contains ([\w ,]+)\)""".r

  private type Ingredient = String
  private type Allergen = String

  override def runDay(input: BufferedSource): Unit = {
    val foods = input.getLines().map {
      case FoodRE(ingredients, allergens) => Food(ingredients.split(' ').toSet, allergens.split(", ").toSet)
    }.toSeq

    val allIngredients = foods.foldLeft(Set.empty[Ingredient])((ingredients, food) => ingredients ++ food.ingredients)
    val allergensMap = foods.foldLeft(Map.empty[Allergen, Set[Ingredient]]) { (map, food) =>
      food.allergens.foldLeft(map) { (currentMap, allergen) =>
        currentMap.updated(allergen, currentMap.getOrElse(allergen, food.ingredients).intersect(food.ingredients))
      }
    }

    val canContainAllergens = allergensMap
      .foldLeft(Set.empty[Ingredient])((ingredients, allergen) => ingredients ++ allergen._2)

    val countOfSafeIngredients = (allIngredients -- canContainAllergens).toSeq.map { ingredient =>
      foods.count(_.ingredients.contains(ingredient))
    }

    printDayPart(1, countOfSafeIngredients.sum, "count of safe ingredients: %s")

    val reducedAllergensMap = reduceToUniqueValues(allergensMap)
    val dangerousIngredients = reducedAllergensMap.keys.toSeq.sorted
      .foldLeft(Vector.empty[Ingredient])((result, key) => result :+ reducedAllergensMap(key))

    printDayPart(2, dangerousIngredients.mkString(","), "dangerous ingredient: %s")
  }

  private case class Food(ingredients: Set[Ingredient], allergens: Set[Allergen])
}
