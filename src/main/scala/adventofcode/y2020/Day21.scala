package adventofcode.y2020

import scala.annotation.tailrec

object Day21 extends Year2020 {
  override val day = 21

  private val FoodRE = """([\w ]+) \(contains ([\w ,]+)\)""".r

  private type Ingredient = String
  private type Allergen = String

  private val foods = input.getLines().map {
    case FoodRE(ingredients, allergens) => Food(ingredients.split(' ').toSet, allergens.split(", ").toSet)
  }.toSeq

  private val allIngredients = foods.foldLeft(Set.empty[Ingredient])((ingredients, food) => ingredients ++ food.ingredients)
  private val allergensMap = foods.foldLeft(Map.empty[Allergen, Set[Ingredient]]) { (map, food) =>
    food.allergens.foldLeft(map) { (currentMap, allergen) =>
      currentMap.updated(allergen, currentMap.getOrElse(allergen, food.ingredients).intersect(food.ingredients))
    }
  }

  private val canContainAllergens = allergensMap
    .foldLeft(Set.empty[Ingredient])((ingredients, allergen) => ingredients ++ allergen._2)

  private val countOfSafeIngredients = (allIngredients -- canContainAllergens).toSeq.map { ingredient =>
    foods.count(_.ingredients.contains(ingredient))
  }

  printDayPart(1, countOfSafeIngredients.sum, "count of safe ingredients: %s")

  private val reducedAllergensMap = reduceIngredients(allergensMap)
  private val dangerousIngredients = reducedAllergensMap.keys.toSeq.sorted
    .foldLeft(Vector.empty[Ingredient])((result, key) => result :+ reducedAllergensMap(key))

  printDayPart(2, dangerousIngredients.mkString(","), "dangerous ingredient: %s")

  @tailrec
  private def reduceIngredients(allergens: Map[Allergen, Set[Ingredient]]): Map[Allergen, Ingredient] = {
    val validForOne = allergens.filter(rule => rule._2.size == 1)
    if (validForOne.size == allergens.size) {
      validForOne.view.mapValues(_.head).toMap
    } else {
      val reduced = allergens.view.mapValues { validFor =>
        if (validFor.size == 1)
          validFor
        else
          validFor.diff(validForOne.values.flatten.toSet)
      }.toMap
      reduceIngredients(reduced)
    }
  }

  private case class Food(ingredients: Set[Ingredient], allergens: Set[Allergen])
}
