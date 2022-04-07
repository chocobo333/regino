
import strformat
import uri

import eat/spanned

type
    Position* = object
        ## quoted from lsp specification.
        line*: Natural      ## Line position in a document (zero-based).
        character*: Natural ## Character offset on a line in a document (zero-based). Assuming that
                            ## the line is represented as a string, the `character` value represents
                            ## the gap between the `character` and `character + 1`.
                            ## If the character value is greater than the line length it defaults back
                            ## to the line length.
    PosRange* = Slice[Position] ## The endpoints are inclusive.
    Location* = object
        uri*: Uri
        `range`*: PosRange

proc `<=`*(self, other: Position): bool =
    if self.line == other.line:
        self.character <= other.character
    else:
        self.line <= other.line

proc `$`*(self: Location): string =
    fmt"{self.uri.path}({self.`range`.a.line+1}, {self.`range`.a.character+1})"
proc `end`*(self: Location): Location =
    Location(
        uri: self.uri,
        `range`: self.`range`.b..self.`range`.b
    )

converter to*(pos: spanned.Position): lineinfos.Position =
    lineinfos.Position(
        line: pos.line - 1,
        character: pos.col - 1
    )

proc newPosition*(line: Natural = 0, character: Natural = 0): Position =
    Position(
        line: line,
        character: character
    )
proc newLocation*(uri: Uri = initUri(), a: Position = newPosition(), b: Position = newPosition()): Location =
    Location(
        uri: uri,
        `range`: a..b
    )

proc toLocation*(self: Spanned, uri: Uri): Location =
    newLocation(uri, self.pos, self.endpos)
proc newLocation*(uri: Uri, a, b: Spanned): Location =
    newLocation(uri, a.pos, b.endpos)
