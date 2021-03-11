
import sugar

import parsers
import nones
import concepts
import utils


proc lookahead*[I, O, E](parser: Parser[I, O, E]): Parser[I, O, E] =
    newParser(
        proc(src: I): Result[I, O, E] =
            parser.parse(src).map((it: (I, O)) => (src, it[1])),
        genGraph("LookAhead", parser),
        true
    )

proc la*[I, O, E](parser: Parser[I, O, E]): Parser[I, O, E] {.inline.} = lookahead(parser)

proc negate*[I, O: Emptiable, E: From[string]](parser: Parser[I, O, E]): Parser[I, O, E] =
    newParser(
        proc(src: I): Result[I, O, E] =
            if parser.parse(src).isOk:
                return err "failed negative lookahead check"
            else:
                return ok (src, O.empty),
        genGraph("Negate", parser),
        true
    )


when isMainModule:
    import patterns
    from combinator import sequence
    echo sequence(lookahead(str[string]"a"), pattern[string]"..").parse("ab")
    echo sequence(la(str[string]"a"), pattern[string]"..")
    echo sequence(la(str[string]"a"), pattern[string]"..").parse("ba")
    echo sequence(negate(str[string]"a"), pattern[string]"..")
    echo sequence(negate(str[string]"a"), pattern[string]"..").parse("ba")
    echo sequence(negate(str[string]"b"), pattern[string]"..").parse("ba")
