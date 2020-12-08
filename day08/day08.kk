module day08

import std/os/file
import std/os/path

import parsers

fun main() {
    val input = read-text-file(path("resources/input.txt"))
    val (result, _) = parse(input, number)
    println(result)
}
