package fwcd.day21

import java.io.File

private val pattern = Regex("([\\w ]+)(?: \\(contains ([^\\)]+)\\))?")

data class Food(
    val ingredients: Set<String>,
    val allergens: Set<String>
)

fun satisfies(food: Food, assignment: Map<String, String?>): Boolean =
    food.allergens.all { allergen -> food.ingredients.any { assignment[it] == allergen } }

fun <T> MutableList<T>.popLast(): T {
    val last = last()
    removeAt(size - 1)
    return last
}

fun solveAllergens(ingreds: List<String>, allergens: MutableSet<String>, foods: MutableList<Food>, assignment: MutableMap<String, String?>): Boolean {
    if (foods.isEmpty()) {
        return allergens.isEmpty()
    } else {
        val food = foods.popLast()

        for (allergen in food.allergens) {
            var found = false
            
            if (allergen in allergens) {
                // Unassigned, try assigning
                for (ingred in food.ingredients) {
                    if (!assignment.containsKey(ingred)) {
                        allergens.remove(allergen)
                        assignment[ingred] = allergen
                        if (solveAllergens(ingreds, allergens, foods, assignment)) {
                            return true
                        }
                        assignment.remove(ingred)
                        allergens.add(allergen)
                    }
                }
            } else {
                found = food.ingredients.any { assignment[it] == allergen }
            }

            if (!found) {
                foods.add(food)
                return false
            }
        }
        
        if (solveAllergens(ingreds, allergens, foods, assignment)) {
            return true
        }
        foods.remove(food)
        return false
    }
}

fun simplify(foods: List<Food>): List<Food> {
    var singles = mutableMapOf<String, Set<String>>()
    
    for (food in foods) {
        for (allergen in food.allergens) {
            val ingreds = food.ingredients.toSet()
            singles[allergen] = singles[allergen]?.also {
                // println("Reinserting $allergen into $singles")
            }?.intersect(ingreds) ?: ingreds
        }
    }
    
    val simplifiedFoods =
        singles.map { Food(it.value, setOf(it.key)) }
    return if (foods.toSet() != simplifiedFoods.toSet()) {
        simplify(simplifiedFoods)
    } else {
        simplifiedFoods
    }
}

fun main(args: Array<String>) {
    val input = File("resources/input.txt").readText()
    val foods = input.lines()
        .mapNotNull(pattern::matchEntire)
        .map { Food(it.groupValues[1].split(" ").toSet(), it.groupValues[2].split(",").map { it.trim() }.toSet()) }
        .sortedBy { -it.allergens.size - it.ingredients.size }
    
    val ingreds = foods.flatMap { it.ingredients }.toSet()
    val allergens = foods.flatMap { it.allergens }.toSet()
    var assignment = mutableMapOf<String, String?>()
    
    var simplified = simplify(foods)
    println("Simpler: $simplified")
    
    if (solveAllergens(ingreds.toList(), allergens.toMutableSet(), simplified.toMutableList(), assignment)) {
        val safeIngreds = ingreds.filterNot { assignment.containsKey(it) }
        println("Safe: $safeIngreds | $assignment >> ${foods.all { satisfies(it, assignment) }}")
        println("Part 1: ${safeIngreds.map { ingred -> foods.map { it.ingredients.count { it == ingred } }.sum() }.sum()}")
        println("Part 2: ${assignment.toList().sortedBy { it.second }.map { it.first }.joinToString(",")}")
    }
}
