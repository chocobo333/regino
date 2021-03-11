
import strformat

import filenames


type
    Position* = object
        line*: int
        col*: int
    LineInfo* = object
        fileId*: FileId         ## indicates identifier for `FileInfo` which is consisted of file name and so on.
        span*: Slice[Position]  ## `span.b` is exclusive while `span.a` is inclusive.

proc newPosition*(line: int = 1, col : int = 1): Position =
    Position(line: line, col: col)

proc `$`*(self: Position): string =
    fmt"({self.line}, {self.col})"

proc newLineInfo*(fileid: FileId , pos, endpos: Position): LineInfo =
    LineInfo(fileId: fileid, span: pos..endpos)

proc `$`*(self: LineInfo): string =
    let filename = if self.fileId == -1:
        "tmp"
    else:
        self.fileId.getFileInfo.filename
    fmt"{filename}{self.span.a}"
