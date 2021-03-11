
import strformat

import parsers
import patterns
import nones
from combinator import alt


proc spaces*[I](min, max: int): Parser[I, I, string] =
    newParser(
        proc(src: I): Result[I, I, string] =
            if min == 0:
                alt(pattern[I](fmt"[[:blank:]]{{1,{max}}}"), none[I, I, string]()).parse(src)
            else:
                pattern[I](fmt"[[:blank:]]{{{min},{max}}}").parse(src),
        fmt"Spaces({min}, {max})"
    )

proc spaces*[I](min: int): Parser[I, I, string] =
    newParser(
        proc(src: I): Result[I, I, string] =
            if min == 0:
                alt(pattern[I](r"[[:blank:]]{1,}"), none[I, I, string]()).parse(src)
            else:
                pattern[I](fmt"[[:blank:]]{{{min},}}").parse(src),
        fmt"Spaces({min})"
    )

proc spaces*[I](min, max: ref int): Parser[I, I, string] =
    newParser(
        proc(src: I): Result[I, I, string] =
            var
                min = min[]
                max = max[]
            if min == 0:
                alt(pattern[I](fmt"[[:blank:]]{{1,{max}}}"), none[I, I, string]()).parse(src)
            else:
                pattern[I](fmt"[[:blank:]]{{{min},{max}}}").parse(src),
        fmt"Spaces({min[]}, {max[]})"
    )

proc spaces*[I](min: ref int): Parser[I, I, string] =
    newParser(
        proc(src: I): Result[I, I, string] =
            var min = min[]
            if min == 0:
                alt(pattern[I](r"[[:blank:]]{1,}"), none[I, I, string]()).parse(src)
            else:
                pattern[I](fmt"[[:blank:]]{{{min},}}").parse(src),
        fmt"Spaces({min[]})"
    )

when isMainModule:
    echo spaces[string](1, 4).parse("       ff")
    echo spaces[string](1).parse("       ff")
    echo spaces[string](1, 1).parse("       ff")
    echo spaces[string](0, 4)
    echo spaces[string](0, 4).parse("ff")
    echo spaces[string](0)
    echo spaces[string](0).parse("ff")