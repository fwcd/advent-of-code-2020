module parsers

// Parser combinators modeled after https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/algeff-tr-2016-v3.pdf

public effect parse {
    fun satisfy(p : (string) -> div maybe<(a, string)>) : a
}

public fun parse(input : string, parser : () -> <div,parse> a) : <div,exn> (a, string) {
    var rest := input

    with fun satisfy(p) {
        match (p(rest)) {
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

// Convenience function over parse that only returns the value
public fun parse-value(input : string, parser : () -> <div,parse> a) : <div,exn> a {
    val (result, _) = parse(input, parser)
    result
}

// Convenience function over parse that uses maybe instead
public fun parse-maybe(input : string, parser : () -> <div,parse> a) : div maybe<(a, string)> {
    try-default(Nothing) { Just(parse(input, parser)) }
}

// Convenience function over parse that only returns the value and uses maybe
public fun parse-value-maybe(input : string, parser : () -> <div,parse> a) : div maybe<a> {
    match (parse-maybe(input, parser)) {
        Just((result, _)) -> Just(result)
        Nothing           -> Nothing
    }
}

public fun word() : parse string {
    satisfy fn(s) {
        match (split(s, " ")) {
            Cons(head, tail) -> Just((head, join(tail, " ")))
            Nil              -> Nothing
        }
    }
}

public fun char(p : char -> bool) : parse char {
    satisfy fn(s) {
        match (head-char(s)) {
            Just(c) | p(c) -> Just((c, tail(s)))
            _              -> Nothing
        }
    }
}

public fun any-char() : parse char {
    char fn(_) { True }
}

public fun digit() : parse char {
    char(is-digit)
}

public fun space() : parse char {
    char fn(c) { c == ' ' }
}

// Parses zero or more repetitions
public fun many(p : () -> <div,parse> a) : <div,parse> list<a> {
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

public fun number() : <div,parse> int {
    // default of -1 should never be needed
    parse-int-default(string(many(digit)), -1)
}

public fun spaces() : <div,parse> string {
    string(many(space))
}
