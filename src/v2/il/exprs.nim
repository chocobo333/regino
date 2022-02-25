
import il
import ../lineinfos


proc literal*(_: typedesc[Expression], val: Literal, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Literal, litval: val, loc: loc)
proc Ident*(_: typedesc[Expression], ident: Ident, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Ident, ident: ident, loc: loc)
proc Pair*(_: typedesc[Expression], first, second: Expression, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Pair, first: first, second: second, loc: loc)
proc Record*(_: typedesc[Expression], members: seq[(string, Expression)], loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Record, members: members, loc: loc)
proc If*(_: typedesc[Expression], elifs: openArray[ElifBranch], elseb: ElseBranch, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.If, elifs: @elifs, elseb: elseb, loc: loc)
proc Case*(_: typedesc[Expression], ofs: seq[OfBranch], default: ElseBranch, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Case, ofs: ofs, default: default, loc: loc)
proc Apply*(_: typedesc[Expression], callee: Expression, args: openArray[Expression], loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Apply, callee: callee, args: @args, loc: loc)
proc Dot*(_: typedesc[Expression], lhs, rhs: Expression, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Dot, lhs: lhs, rhs: rhs, loc: loc)
proc Bracket*(_: typedesc[Expression], callee: Expression, args: openArray[Expression], loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Bracket, callee: callee, args: @args, loc: loc)
proc Malloc*(_: typedesc[Expression], mtype: Expression, msize: Expression, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Malloc, mtype: mtype, msize: msize, loc: loc)
