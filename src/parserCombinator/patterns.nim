
import strformat
import strutils
import options

import nre

import parsers
import reutils
import optionmaps
import concepts
import utils


proc taken*(self: string, val: string): Option[(string, string)] =
    if self.startsWith(val):
        return some (self[val.len..^1], val)
    else:
        none (string, string)

proc str*[T: Takenable](bytes: string): Parser[T, T, string] =
    newParser(
        proc(src: T): Result[T, T, string] =
            # TODO: mixin or bind?
            bind taken
            let tmp = src.taken(bytes)
            if tmp.isSome:
                ok tmp.get
            else:
                err(fmt"NotMatch @ str""{bytes}"""),
        genGraph("Bytes", bytes.escape)
        # "Bytes " & bytes.escape
    )

proc takeMatch*(src: string, pattern: string): Result[string, string, string] =
    let
        p = re(fmt"(*UTF8){pattern}")
    match(src, p).filterIt(it.match != "").mapIt((it.rest, it.match), fmt"NotMatch @ pattern""{pattern}""")

proc pattern*[T: Matchable](pattern: string): Parser[T, T, string] =
    newParser(
        proc(src: T): Result[T, T, string] =
            # TODO: mixin or bind?
            bind takeMatch
            src.takeMatch(pattern),
        genGraph("Pattern", pattern.escape)
        # "Pattern " & pattern.escape
    )


when isMainModule:
    echo str[string]("ff")
    assert str[string]("ff").parse("ffg") == ok(Result[string, string, string], ("g", "ff"))
    assert pattern[string]("a.c.").parse("abc®aiuec") == ok(Result[string, string, string], ("aiuec", "abc®"))
    echo pattern[string]"a.c."
    echo str[string]"ff"
    assert pattern[string]("b*").parse("aaa") == err(Result[string, string, string], "NotMatch @ pattern\"b*\"")