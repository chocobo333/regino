
import options

import il
import ../lineinfos


proc literal*(_: typedesc[Expression], val: Literal, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Literal, litval: val, loc: loc)
proc Id*(_: typedesc[Expression], ident: Ident, loc: Location = ident.loc): Expression =
    Expression(kind: ExpressionKind.Ident, ident: ident, loc: loc)
proc Tuple*(_: typedesc[Expression], exprs: seq[Expression], loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Tuple, exprs: exprs, loc: loc)
proc Seq*(_: typedesc[Expression], exprs: seq[Expression], loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Seq, exprs: exprs, loc: loc)
proc Record*(_: typedesc[Expression], members: seq[(Ident, Expression)], loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Record, members: members, loc: loc)
proc If*(_: typedesc[Expression], elifs: openArray[ElifBranch], elseb: Option[ElseBranch], loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.If, elifs: @elifs, elseb: elseb, loc: loc)
proc When*(_: typedesc[Expression], elifs: openArray[ElifBranch], elseb: Option[ElseBranch], loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.When, elifs: @elifs, elseb: elseb, loc: loc)
proc Case*(_: typedesc[Expression], ofs: seq[OfBranch], default: ElseBranch, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Case, ofs: ofs, default: default, loc: loc)
proc Call*(_: typedesc[Expression], callee: Expression, args: openArray[Expression], loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Call, callee: callee, args: @args, loc: loc)
proc Command*(_: typedesc[Expression], callee: Expression, args: openArray[Expression], loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Command, callee: callee, args: @args, loc: loc)
proc Dot*(_: typedesc[Expression], lhs, rhs: Expression, loc: Location = newLocation(lhs.loc.uri, lhs.loc.range.a, rhs.loc.range.b)): Expression =
    Expression(kind: ExpressionKind.Dot, lhs: lhs, rhs: rhs, loc: loc)
proc Binary*(_: typedesc[Expression], op: Ident, lhs, rhs: Expression, loc: Location = newLocation(lhs.loc.uri, lhs.loc.range.a, rhs.loc.range.b)): Expression =
    Expression(kind: ExpressionKind.Binary, op: op, lhs: lhs, rhs: rhs, loc: loc)
proc Prefix*(_: typedesc[Expression], op: Ident, expression: Expression, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Prefix, op: op, expression: expression, loc: loc)
proc Postfix*(_: typedesc[Expression], op: Ident, expression: Expression, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Postfix, op: op, expression: expression, loc: loc)
proc Bracket*(_: typedesc[Expression], callee: Expression, args: openArray[Expression], loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Bracket, callee: callee, args: @args, loc: loc)
proc Lambda*(_: typedesc[Expression], param: (seq[IdentDef], Option[Expression]), body: Suite, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Lambda, param: FunctionParam(params: param[0], rety: param[1]), body: body, loc: loc)
proc Malloc*(_: typedesc[Expression], mtype: Expression, msize: Expression, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Malloc, mtype: mtype, msize: msize, loc: loc)
proc Typeof*(_: typedesc[Expression], `typeof`: Expression, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Typeof, `typeof`: `typeof`, loc: loc)
proc Ref*(_: typedesc[Expression], `ref`: Expression, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Ref, `ref`: `ref`, loc: loc)
proc FnType*(_: typedesc[Expression], args: openArray[Expression], rety: Expression, loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.FnType, rety: rety, args: @args, loc: loc)
proc Fail*(_: typedesc[Expression], loc: Location = newLocation()): Expression =
    Expression(kind: ExpressionKind.Fail, loc: loc)

proc empty*(_: typedesc[Expression]): Expression = Expression.Fail
