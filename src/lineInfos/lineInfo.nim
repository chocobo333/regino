
import strformat

import filenames
import eat/spanned


type
    LineInfo* = object
        fileId*: FileId         ## indicates identifier for `FileInfo` which is consisted of file name and so on.
        span*: Slice[Position]  ## `span.b` is exclusive while `span.a` is inclusive.

proc newLineInfo*(fileid: FileId , pos, endpos: Position): LineInfo =
    LineInfo(fileId: fileid, span: pos..endpos)

proc `$`*(self: LineInfo): string =
    let filename = if self.fileId == -1:
        "tmp"
    else:
        self.fileId.getFileInfo.filename
    fmt"{filename}{self.span.a}"

proc toLineInfo*(self: Spanned, fileid: FileId): LineInfo =
    newLineInfo(fileid, self.pos, self.endpos)
proc newLineInfo*(fileid: FileId, a, b: Spanned): LineInfo =
    newLineInfo(fileid, a.pos, b.endpos)