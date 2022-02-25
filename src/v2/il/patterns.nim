
import il

proc Literal*(_: typedesc[Pattern], litval: Literal): Pattern =
    Pattern(kind: PatternKind.Literal, litval: litval)
proc Ident*(_: typedesc[Pattern], ident: Ident): Pattern =
    Pattern(kind: PatternKind.Ident, ident: ident)
proc Dot*(_: typedesc[Pattern], lhs: Pattern, rhs: Ident): Pattern =
    Pattern(kind: PatternKind.Dot, lhs: Pattern, rhs: Ident)
proc Bracket*(_: typedesc[Pattern], callee: Pattern, args: openArray[Expression]): Pattern =
    Pattern(kind: PatternKind.Bracket, callee: callee, args: @args)
proc Pair*(_: typedesc[Pattern], first, second: Pattern): Pattern =
    Pattern(kind: PatternKind.Pair, first: first, second: second)
proc UnderScore*(_: typedesc[Pattern]): Pattern =
    Pattern(kind: PatternKind.UnderScore)
