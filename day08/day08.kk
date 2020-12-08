import std/os/file
import std/os/path

// Parser combinators modeled after https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/algeff-tr-2016-v3.pdf

effect parse {
    fun satisfy(p : (string) -> div maybe<(a, string)>) : a
}

fun parse(input : string, parser : () -> <div,parse> a) : <div,exn> (a, string) {
    var rest := input

    with fun satisfy(p) {
        match (p(input)) {
            Nothing      -> throw("Long word, no parse :( (failed while parsing '" + input + "')")
            Just((x, r)) -> {
                rest := r
                x
            }
        }
    }

    val result = parser()
    return (result, rest)
}

// Convenience function over parse that uses maybe instead
fun parse-maybe(input : string, parser : () -> <div,parse> a) : div maybe<(a, string)> {
    try-default(Nothing) { Just(parse(input, parser)) }
}

fun word() : parse string {
    satisfy fn(s) {
        match (split(s, " ")) {
            Cons(head, tail) -> Just((head, join(tail, " ")))
            Nil              -> Nothing
        }
    }
}

fun char(p : char -> bool) : parse char {
    satisfy fn(s) {
        match (head-char(s)) {
            Just(c) | p(c) -> Just((c, tail(s)))
            _              -> Nothing
        }
    }
}

fun anyChar() : parse char {
    char fn(_) { True }
}

fun digit() : parse char {
    char(is-digit)
}

// Parses zero or more repetitions
fun many(p : () -> <div,parse> a) : <div,parse> list<a> {
    fun f(s) {
        match (parse-maybe(s, p)) {
            Just((x, rest)) -> {
                val (xs, rest2) = f(rest)
                (Cons(x, xs), rest2)
            }
            Nothing         -> (Nil, s)
        }
    }
    satisfy fn(s) { Just(f(s)) }
}

fun number() : <div,parse> int {
    // default of -1 should never be needed
    parse-int-default(string(many(digit)), -1)
}

fun main() {
    val input = read-text-file(path("resources/input.txt"))
    val (result, _) = parse(input, number)
    println(result)
}
