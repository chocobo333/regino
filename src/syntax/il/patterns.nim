
import sequtils

import il
import ../../utils


proc literal*(_: typedesc[Pattern], litval: Literal): Pattern =
    Pattern(kind: PatternKind.Literal, litval: litval)
proc Id*(_: typedesc[Pattern], ident: Ident): Pattern =
    Pattern(kind: PatternKind.Ident, ident: ident)
proc Tuple*(_: typedesc[Pattern], patterns: seq[Pattern]): Pattern =
    Pattern(kind: PatternKind.Tuple, patterns: patterns)
proc Record*(_: typedesc[Pattern], members: seq[(Ident, Pattern)]): Pattern =
    Pattern(kind: PatternKind.Record, members: members)
proc UnderScore*(_: typedesc[Pattern]): Pattern =
    Pattern(kind: PatternKind.UnderScore)

proc empty*(_: typedesc[Pattern]): Pattern =
    Pattern.UnderScore

proc refutable*(self: Pattern): bool =
    case self.kind
    of PatternKind.Literal:
        false
    of PatternKind.Ident:
        true
    of PatternKind.Tuple:
        self.patterns.foldl(a and b.refutable, true)
    of PatternKind.Record:
        self.members.foldl(a and b[1].refutable, true)
    of PatternKind.UnderScore:
        true


proc collectIdent*(self: Pattern): seq[Ident] =
    case self.kind:
    of PatternKind.Literal:
        @[]
    of PatternKind.Ident:
        @[self.ident]
    of PatternKind.Tuple:
        self.patterns.mapIt(it.collectIdent).flatten
    of PatternKind.Record:
        self.members.mapIt(it[1].collectIdent).flatten
    of PatternKind.UnderScore:
        @[]
