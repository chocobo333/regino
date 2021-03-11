
import strformat
import strutils
import options
import unicode

import nre

import lineInfo
import filenames


type
    # TODO: Generics?
    Spanned* = object
        fragment*: string
        pos*: Position
        endpos*: Position

proc toLineInfo*(self: Spanned, fileid: FileId): LineInfo =
    newLineInfo(fileid, self.pos, self.endpos)
proc newLineInfo*(fileid: FileId, a, b: Spanned): LineInfo =
    newLineInfo(fileid, a.pos, b.endpos)

proc newSpanned*(frag: string, loc: Position, endloc: Position): Spanned =
    Spanned(fragment: frag, pos: loc, endpos: endloc)

converter toSpanned*(s: string): Spanned =
    let
        lines = splitLines(s)
    newSpanned(s, newPosition(), newPosition(lines.len, lines[^1].len+1))

proc `$`*(self: Spanned): string =
    &"\"{self.fragment}\"{self.pos}"

proc add*(self: var Spanned, s: string) =
    self.fragment.add s


import ../parserCombinator except p, s


proc empty*(self: typedesc[Spanned]): Spanned =
    Spanned(fragment: "", pos: newPosition())

proc startsWith(self: Spanned, val: string): bool = self.fragment.startsWith(val)

proc taken(self: Spanned, val: string): Option[(Spanned, Spanned)] =
    if self.startsWith(val):
        let
            n = val.len
            lines = splitLines(val)
            newpos = if lines.len == 1:
                newPosition(self.pos.line, self.pos.col + lines[0].runeLen)
            else:
                newPosition(self.pos.line+1, lines[^1].runeLen + 1)
        some (
            newSpanned(self.fragment[n..^1], newpos, self.endpos),
            newSpanned(self.fragment[0..<n], self.pos, newpos)
        )
    else:
        none (Spanned, Spanned)

type
    RegexMatchSpanned = object
        res: RegexMatch
        pos: Position
        endpos: Position

proc newRegexMatchSpanned(res: Option[RegexMatch], location: Position, endloc: Position): Option[RegexMatchSpanned] =
    if res.isSome:
        some RegexMatchSpanned(res: res.get, pos: location, endpos: endloc)
    else:
        none RegexMatchSpanned

proc match(src: Spanned, pattern: Regex): Option[RegexMatchSpanned] =
    newRegexMatchSpanned(
        match(src.fragment, pattern),
        src.pos,
        src.endpos
    )

proc match(src: RegexMatchSpanned): Spanned =
    newSpanned(src.res.match, src.pos, src.endpos)

proc rest(src: RegexMatchSpanned): Spanned =
    newSpanned(src.res.str[src.res.matchBounds.b+1..^1], src.pos, src.endpos)

proc restnMatch(src: RegexMatchSpanned): (Spanned, Spanned) =
    let
        s = src.res.str
        m = src.res.match
        lines = splitLines(m)
        newpos = if lines.len == 1:
            newPosition(src.pos.line, src.pos.col + lines[0].runeLen)
        else:
            newPosition(src.pos.line+1, lines[^1].runeLen + 1)
    (
        newSpanned(s[src.res.matchBounds.b+1..^1], newpos, src.pos),
        newSpanned(m, src.pos, newpos)
    )

proc takeMatch(src: Spanned, pattern: string): Result[Spanned, Spanned, string] =
    let
        p = re(fmt"(*UTF8){pattern}")
    match(src, p).filterIt(it.match.fragment != "").mapIt(it.restnMatch, fmt"got ""{src.fragment[0..min(src.fragment.len-1, 8)]}"", but expect for pattern""{pattern}""")

proc s*(bytes: string): Parser[Spanned, Spanned, string] = str[Spanned](bytes)
proc p*(pat: string): Parser[Spanned, Spanned, string] = pattern[Spanned](pat)
proc sp*(min: int): Parser[Spanned, Spanned, string] = spaces[Spanned](min)
proc sp*(min, max: int): Parser[Spanned, Spanned, string] = spaces[Spanned](min, max)
proc pos*(): Parser[Spanned, Spanned, string] = parserCombinator.none[Spanned, Spanned, string]()

when isMainModule:
    import sugar

    # let
    #     parser1 = s"ff"
    #     parser2 = s("ff\nff")
    #     parser3 = p"a.c."
    # echo parser1
    # echo parser2
    # echo parser3

    # echo parser1.parse("ffgg")
    # echo parser2.parse("ff\nffee")
    # echo parser3.parse("abc®d")

    let
        parser1 = s"e" \ p".." \ p"..."
        parser2 = s"ff" + p".."
        parser3 = s"ff" > -p".." > p"..." @ ((it: seq[Spanned]) => (result = it[0];for e in it[1..^1]: result.add e.fragment))
        parser4 = p"[a-z]" ^+ s","
    echo parser1
    echo parser2
    echo parser3
    echo parser4
    echo parser1.parse("ffdefabc")
    echo parser2.parse("ffdefabc")
    echo parser3.parse("ffdefabc")
    echo separated1(s"a", s(",")).parse("a,a,a,a")
    echo parser4.parse("a,b,c")
    echo (s"a" * 5)
    echo (s"a" * 5).parse("aaaaaaa")
    echo (s"a" * 5).parse("aabaaaa")
    echo (*s"a").parse("aaaaaaa")
    echo (*s"b").parse("aaaaaaa")
    echo (+s"b")
    echo (+s"b").parse("aaaaaaa")
    echo delimited(s"(", p"[0-9]".fold0(0, (acc, n)=>acc+parseInt(n.fragment)), s")")
    echo delimited(s"(", p"[0-9]".fold0(0, (acc, n)=>acc+parseInt(n.fragment)), s")").parse("(12345)")

