
import options

import il


proc literal*(_: typedesc[Pattern], litval: Literal): Pattern =
    Pattern(kind: PatternKind.Literal, litval: litval)
proc Id*(_: typedesc[Pattern], ident: Ident, index: Option[Expression] = none(Expression)): Pattern =
    Pattern(kind: PatternKind.Ident, ident: ident, index: index)
proc Dot*(_: typedesc[Pattern], lhs: Pattern, rhs: Ident): Pattern =
    Pattern(kind: PatternKind.Dot, lhs: Pattern, rhs: Ident)
proc Bracket*(_: typedesc[Pattern], callee: Pattern, args: openArray[Expression]): Pattern =
    Pattern(kind: PatternKind.Bracket, callee: callee, args: @args)
proc Tuple*(_: typedesc[Pattern], patterns: seq[Pattern]): Pattern =
    Pattern(kind: PatternKind.Tuple, patterns: patterns)
proc Record*(_: typedesc[Pattern], members: seq[(string, Pattern)]): Pattern =
    Pattern(kind: PatternKind.Record, members: members)
proc UnderScore*(_: typedesc[Pattern]): Pattern =
    Pattern(kind: PatternKind.UnderScore)
