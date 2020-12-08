import std/os/file
import std/os/path

// Parser combinators modeled after https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/algeff-tr-2016-v3.pdf

effect parse {
    fun satisfy(p : string -> maybe<(a, string)>) : a
}

fun parse(input : string, parser : () -> parse a) : exn a {
    with fun satisfy(p) {
        match (p(input)) {
            Nothing      -> throw("Long word, no parse :( (failed while parsing '" + input + "')")
            Just((x, _)) -> x
        }
    }

    parser()
}

fun word() : parse string {
    satisfy fn(s) {
        match (split(s, " ")) {
            Cons(head, tail) -> Just((head, join(tail, " ")))
            Nil              -> Nothing
        }
    }
}

fun main() {
    val input = read-text-file(path("resources/input.txt"))
    throw("Whoops")
    println(parse("", word))
}
