package fwcd.day21

import java.io.File

private val pattern = Regex("([\\w ]+)(?: \\(contains ([\\w,]+)\\))?")

data class Food(
    val ingredients: List<String>,
    val allergens: List<String>
)

fun main(args: Array<String>) {
    val input = File("resources/example.txt").readText()
    val foods = input.lines()
        .mapNotNull(pattern::matchEntire)
        .map { Food(it.groupValues[1].split(" "), it.groupValues[2].split(" ")) }
    
    print(foods)
}
