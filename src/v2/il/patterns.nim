
import options
import sequtils

import il


proc literal*(_: typedesc[Pattern], litval: Literal): Pattern =
    Pattern(kind: PatternKind.Literal, litval: litval)
proc Id*(_: typedesc[Pattern], ident: Ident, index: Option[Expression] = none(Expression)): Pattern =
    Pattern(kind: PatternKind.Ident, ident: ident, index: index)
proc Dot*(_: typedesc[Pattern], lhs: Pattern, rhs: Ident): Pattern =
    Pattern(kind: PatternKind.Dot, lhs: Pattern, rhs: Ident)
# proc Bracket*(_: typedesc[Pattern], callee: Pattern, args: openArray[Expression]): Pattern =
#     Pattern(kind: PatternKind.Bracket, callee: callee, args: @args)
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
    of PatternKind.Dot:
        true
    of PatternKind.Tuple:
        self.patterns.foldl(a and b.refutable, true)
    of PatternKind.Record:
        self.members.foldl(a and b[1].refutable, true)
    of PatternKind.UnderScore:
        true
