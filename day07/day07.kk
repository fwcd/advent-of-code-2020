import std/os/file
import std/os/path

fun main() {
    val input = read-text-file(path("resources/input.txt"))
    println(join(lines(input), " "))
}
