
import parserCombinator/[
    patterns,
    repeats,
    combinator,
    spaces,
    lookaheads,
    recovers,
    drop,
    nones
]
export
    patterns,
    repeats,
    combinator,
    spaces,
    lookaheads,
    recovers,
    drop,
    nones


import parserCombinator/[
    parsers,
    concepts,
    optionmaps,
]
import
    sugar,
    strutils,
    options

export parsers
export optionmaps


func s*(bytes: string): Parser[string, string, string] =
    str[string](bytes)

func p*(pat: string): Parser[string, string, string] =
    pattern[string](pat)

proc `\`*[I, O, E](a: varargs[Parser[I, O, E]]): Parser[I, O, E] {.inline.} =
    alt(@a)
proc `+`*[I, O1, O2, E](a: Parser[I, O1, E], b: Parser[I, O2, E]): Parser[I, (O1, O2), E] {.inline.} =
    tupl(a, b)
proc `>`*[I, O, E](a, b: Parser[I, O, E]): Parser[I, seq[O], E] {.inline.} =
    sequence(@[a, b])
proc `>`*[I, O, E](a: Parser[I, seq[O], E], b: Parser[I, O, E]): Parser[I, seq[O], E] {.inline.} =
    if a.dropped:
        tupl(a, b).map(it => @[it[1]])
    else:
        tupl(a, b).map(it => it[0] & it[1])
proc `@`*[I, O1, O2, E](a: Parser[I, O1, E], callback: O1 -> O2): Parser[I, O2, E] {.inline.} =
    a.map(callback)
proc `@@`*[I, O1, O2, E](a: Parser[I, O1, E], callback: Result[I, O1, E] -> Result[I, O2, E]): Parser[I, O2, E] {.inline.} =
    a.mapRes(callback)
proc `?`*[I, O, E](parser: Parser[I, O, E]): Parser[I, Option[O], E] {.inline.} =
    opt(parser)
proc `-`*[I, O, E](a: Parser[I, O, E]): Parser[I, O, E] {.inline.} =
    a.drop
proc `^+`*[I, O1, O2, E](parser: Parser[I, O1, E], delimiter: Parser[I, O2, E]): Parser[I, seq[O1], E] {.inline.} =
    separated1(parser, delimiter)
proc `^*`*[I, O1, O2, E](parser: Parser[I, O1, E], delimiter: Parser[I, O2, E]): Parser[I, seq[O1], E] {.inline.} =
    separated0(parser, delimiter)
proc `&`*[I, O, E](parser: Parser[I, O, E]): Parser[I, O, E] {.inline.} =
    lookahead(parser)
proc `!`*[I, O: Emptiable, E: string | From[string]](parser: Parser[I, O, E]): Parser[I, O, E] {.inline.} =
    negate(parser)
proc `\=`*[I, O, E](parser: Parser[I, O, E], rcvr: Parser[I, O, E]): Parser[I, O, E] {.inline.} =
    recover(parser, rcvr)
proc `*`*[I, O, E](parser: Parser[I, O, E]): Parser[I, seq[O], E] {.inline.} =
    many0(parser)
proc `+`*[I, O, E](parser: Parser[I, O, E]): Parser[I, seq[O], E] {.inline.} =
    many1(parser)
proc `*`*[I, O, E](parser: Parser[I, O, E], n: int): Parser[I, seq[O], E] {.inline.} =
    times(parser, n)

proc `^`*[I, O1, O2, E](inner: Parser[I, O1, E], outer: Parser[I, O2, E]): Parser[I, O1, E] {.inline.} =
    surrounded(outer, inner)

template optAlt*{`\` * a}[I, O, E](a: Parser[I, O, E]): untyped = `\`(a)

template optTerminate*{`+`(a, `-`(b))}[I, O1, O2, E](a: Parser[I, O1, E], b: Parser[I, O2, E]): untyped =
    terminated(a, b)

# proc `>>`*[I, O, E](a: varargs[Parser[I, O, E]]): Parser[I, seq[O], E] =
#     sequence(@a)
# template optSeq*{`>>` * a}[I, O, E](a: Parser[I, O, E]): untyped = `>>`(a)
    



when isMainModule:
    echo Parser[string, string, string] is To[string]
    let
        parser = s"e" \ p".." \ p"..."
        parser2 = s"ff" + p".."
        parser3 = s"ff" > -p".." > p"..." @ (it => (result = "";for e in it: result.add e))
        parser4 = p"[a-z]" ^+ -s","
    echo parser
    echo parser2
    echo parser3
    echo parser4
    echo parser.parse("ffdefabc")
    echo parser2.parse("ffdefabc")
    echo parser3.parse("ffdefabc")
    echo separated1(str[string]"a", str[string](",").drop).parse("a,a,a,a")
    echo parser4.parse("a,b,c")
    echo (s"a" * 5)
    echo (s"a" * 5).parse("aaaaaaa")
    echo (s"a" * 5).parse("aabaaaa")
    echo (*s"a").parse("aaaaaaa")
    echo (*s"b").parse("aaaaaaa")
    echo (+s"b")
    echo (+s"b").parse("aaaaaaa")
    echo delimited(s"(", p"[0-9]".fold0(0, (acc, n)=>acc+parseInt(n)), s")")
    echo delimited(s"(", p"[0-9]".fold0(0, (acc, n)=>acc+parseInt(n)), s")").parse("(12345)")
    echo s"ff" ^ spaces[string](0)
    echo (s"ff" ^ spaces[string](0)).parse("  ff")
    echo (s"ff" + -s"ee").parse("ffee")