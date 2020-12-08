module day08

import std/os/file
import std/os/path

import parsers

struct instruction {
    op : string
    value : int
}

fun pretty(inst : instruction) : string {
    inst.op + " " + show(inst.value)
}

fun instruction() : <div,parse> instruction {
    val op = word()
    spaces()
    val sign = any-char()
    val value = number() * (if (sign == '-') then -1 else 1)
    Instruction(op, value)
}

effect machine {
    fun jmp(delta : int) : ()

    fun acc(delta : int) : ()

    fun nop() : ()
}

fun run(inst : instruction) : <exn,machine> () {
    match (inst.op) {
        "acc" -> acc(inst.value)
        "jmp" -> jmp(inst.value)
        "nop" -> nop()
        _     -> throw("Unknown instruction: " + inst.op)
    }
}

fun part1(prog : list<instruction>) : <div,exn> int {
    var previous-ips := []
    var ip := 0
    var value := 0

    fun move-ip(delta : int) {
        previous-ips := Cons(ip, previous-ips)
        ip := ip + delta
    }

    with {
        fun jmp(delta) {
            move-ip(delta)
        }
        fun acc(delta) {
            value := value + delta
            move-ip(1)
        }
        fun nop() {
            move-ip(1)
        }
    }

    while { all(previous-ips) fn(p) { p != ip } } {
        match (prog[ip]) {
            Just(inst) -> run(inst)
            Nothing    -> throw("No instruction left :(")
        }
    }

    value
}

fun main() {
    val input = read-text-file(path("resources/input.txt"))
    val instructions = concat-maybe(map(lines(input)) fn(l) { parse-value-maybe(l, instruction) })

    println("Part 1: " + show(part1(instructions)))
}
