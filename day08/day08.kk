module day08

import std/os/file
import std/os/path

import parsers

struct instruction {
    op : string
    value : int
}

fun instruction() : <div,parse> instruction {
    val op = word()
    spaces()
    val sign = any-char()
    val value = number() * (if (sign == '-') then -1 else 1)
    Instruction(op, value)
}

fun main() {
    val input = read-text-file(path("resources/input.txt"))
    val (result, _) = parse(input, number)
    println(result)
}
