
import uri

import ../../lineinfos


type
    ParseErrorKind* = enum
        Expected
        InvalidIndentation
    ParseError* = object
        loc*: Location
        msg*: seq[string]
        kind*: ParseErrorKind
    Source* = object
        src*: string
        line*: Natural
        character*: Natural
        abs*: Natural
        uri*: Uri
        indent*: seq[int]
        errs*: seq[ParseError]


proc `$`*(self: ref Source): string =
    self.src

proc `from`*(_: typedesc[Source], str: string, uri: string = ""): ref Source =
    new result
    result[] = Source(
        src: str,
        line: 0,
        character: 0,
        abs: 0,
        uri: uri.parseUri,
        indent: @[0],
        errs: @[]
    )

proc open*(_: typedesc[Source], filename: string): ref Source =
    new result
    let
        f = open(filename)
    defer:
        f.close()
    result[] = Source(
        src: f.readAll,
        line: 0,
        character: 0,
        abs: 0,
        uri: filename.parseUri,
        indent: @[0],
        errs: @[]
    )

proc take*(self: ref Source, s: string) =
    for c in s:
        self.abs += 1
        if c == '\n':
            self.line += 1
            self.character = 0
        else:
            self.character += 1
    self.src = self.src[s.len..^1]
