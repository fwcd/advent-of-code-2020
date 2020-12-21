package fwcd.day21

import java.io.File

private val pattern = Regex("([\\w ]+)(?: \\(contains ([^\\)]+)\\))?")

data class Food(
    val ingredients: List<String>,
    val allergens: List<String>
)

fun satisfies(foods: List<Food>, assignment: Map<String, String?>): Boolean =
    foods.all { food -> food.allergens.all { allergen -> food.ingredients.any { assignment[it] == allergen } } }

fun solveAllergens(ingreds: List<String>, allergens: List<String>, foods: List<Food>, assignment: MutableMap<String, String?>): Boolean {
    if (ingreds.isEmpty()) {
        return satisfies(foods, assignment)
    } else {
        val ingred = ingreds.last()
        for (allergen in listOf(null) + allergens.filterNot { assignment.values.contains(it) }) {
            assignment[ingred] = allergen
            if (solveAllergens(ingreds.dropLast(1), allergens, foods, assignment)) {
                return true
            }
        }
        return false
    }
}

fun main(args: Array<String>) {
    val input = File("resources/example.txt").readText()
    val foods = input.lines()
        .mapNotNull(pattern::matchEntire)
        .map { Food(it.groupValues[1].split(" "), it.groupValues[2].split(",").map { it.trim() }) }
    
    val ingreds = foods.flatMap { it.ingredients }.toSet()
    val allergens = foods.flatMap { it.allergens }.toSet()
    var assignment = mutableMapOf<String, String?>()
    
    if (solveAllergens(ingreds.toList(), allergens.toList(), foods, assignment)) {
        println("$assignment")
        val safeIngreds = assignment.filter { it.value == null }.map { it.key }
        println("Part 1: ${safeIngreds.map { ingred -> foods.map { it.ingredients.count { it == ingred } }.sum() }.sum()}")
    }
}
