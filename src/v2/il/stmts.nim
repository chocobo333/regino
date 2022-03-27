
import options

import il
import ../lineinfos


proc For*(_: typedesc[Statement], pat: Pattern, val: Expression, suite: Suite, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Asign, pat: pat, val: val, suite: suite, loc: loc)
proc While*(_: typedesc[Statement], branch: ElifBranch, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.While, branch: branch, loc: loc)
proc Loop*(_: typedesc[Statement], `block`: Suite, label: Option[Ident] = none(Ident), loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Loop, `block`: `block`, label: label, loc: loc)
proc LetSection*(_: typedesc[Statement], iddefs: openarray[IdentDef], loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.LetSection, iddefs: @iddefs, loc: loc)
proc VarSection*(_: typedesc[Statement], iddefs: openarray[IdentDef], loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.VarSection, iddefs: @iddefs, loc: loc)
proc ConstSection*(_: typedesc[Statement], iddefs: openarray[IdentDef], loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.ConstSection, iddefs: @iddefs, loc: loc)
proc TypeSection*(_: typedesc[Statement], typedefs: openarray[TypeDef], loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.TypeSection, typedefs: @typedefs, loc: loc)
proc Asign*(_: typedesc[Statement], pat: Pattern, op: Ident, val: Expression, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Asign, pat: pat, op: op, val: val, loc: loc)
proc Funcdef*(_: typedesc[Statement], fn: Function, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Funcdef, fn: fn, loc: loc)
proc Meta*(_: typedesc[Statement], meta: Metadata, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Meta, meta: meta, loc: loc)
proc Discard*(_: typedesc[Statement], dis: Option[Expression] = none(Expression), loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Discard, `discard`: dis, loc: loc)
proc Comments*(_: typedesc[Statement], comments: seq[string], loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Comments, comments: comments, loc: loc)
proc Expr*(_: typedesc[Statement], exp: Expression, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Expression, expression: exp, loc: loc)
proc Fail*(_: typedesc[Statement], loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Fail, loc: loc)

converter toStmt*(self: Expression): Statement =
    Statement.Expr(self)

proc empty*(_: typedesc[Statement]): Statement = Statement.Fail
