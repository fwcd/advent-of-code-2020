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

fun interpret(prog : list<instruction>) : <div,exn> (int, bool) {
    var previous-ips := []
    var ip := 0
    var value := 0
    var terminated := False

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

    // Run until program either terminates or runs into endless loop
    while { !terminated && all(previous-ips) fn(p) { p != ip } } {
        match (prog[ip]) {
            Just(inst) -> run(inst)
            Nothing    -> terminated := True
        }
    }

    (value, terminated)
}

fun replace-nth(xs : list<a>, i : int, x : a) : list<a> {
    map-indexed(xs) fn(j, y) {
        if (i == j) x else y
    }
}

fun potential-fixes(prog : list<instruction>) : console list<list<instruction>> {
    concat-maybe(map-indexed(prog) fn(i, inst) {
        match (inst.op) {
            "nop" -> Just(replace-nth(prog, i, Instruction("jmp", inst.value)))
            "jmp" -> Just(replace-nth(prog, i, Instruction("nop", inst.value)))
            _     -> Nothing
        }
    })
}

fun main() {
    val input = read-text-file(path("resources/input.txt"))
    // Not declared as a variable due to a compiler bug that causes a segfault
    // if interpret is called with the same list of instructions multiple times
    fun instructions() { concat-maybe(map(lines(input)) fn(l) { parse-value-maybe(l, instruction) }) }

    val (part1, _) = interpret(instructions())
    println("Part 1: " + show(part1))

    val fixes = potential-fixes(instructions())
    val part2 = filter-map(fixes) fn(prog) {
        val (result, terminated) = interpret(prog)
        if (terminated) Just(result) else Nothing
    }
    println("Part 2: " + show(part2))
}
