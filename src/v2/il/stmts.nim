
import options

import il
import ../lineinfos


proc Block*(_: typedesc[Statement], `block`: Suite, label: Option[Ident] = none(Ident), loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Block, `block`: `block`, label: label, loc: loc)
proc For*(_: typedesc[Statement], pat: Pattern, val: Expression, suite: Suite, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Asign, pat: pat, val: val, suite: suite, loc: loc)
proc While*(_: typedesc[Statement], branch: ElifBranch, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.While, branch: branch, loc: loc)
proc Loop*(_: typedesc[Statement], `block`: Suite, label: Option[Ident] = none(Ident), loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Loop, `block`: `block`, label: label, loc: loc)
proc Let*(_: typedesc[Statement], iddef: IdentDef, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Let, iddef: iddef, loc: loc)
proc Var*(_: typedesc[Statement], iddef: IdentDef, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Var, iddef: iddef, loc: loc)
proc Const*(_: typedesc[Statement], iddef: IdentDef, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Const, iddef: iddef, loc: loc)
proc Typedef*(_: typedesc[Statement], iddefs: openarray[IdentDef], loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Typedef, iddefs: @iddefs, loc: loc)
proc Asign*(_: typedesc[Statement], pat: Pattern, val: Expression, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Asign, pat: pat, val: val, loc: loc)
proc Funcdef*(_: typedesc[Statement], fn: Function, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Funcdef, fn: fn, loc: loc)
proc Meta*(_: typedesc[Statement], meta: Metadata, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Meta, meta: meta, loc: loc)
proc Expr*(_: typedesc[Statement], exp: Expression, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Expression, expression: exp, loc: loc)

converter toStmt*(self: Expression): Statement =
    Statement.Expr(self)
