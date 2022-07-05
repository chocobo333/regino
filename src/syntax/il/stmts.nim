
import options

import il
import ../../lineinfos


proc Import*(_: typedesc[Statement], module: Ident, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Import, module: module, loc: loc)
proc For*(_: typedesc[Statement], pat: Pattern, val: Expression, suite: Suite, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Asign, pat: pat, val: val, suite: suite, loc: loc)
proc While*(_: typedesc[Statement], branch: ElifBranch, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.While, branch: branch, loc: loc)
proc Loop*(_: typedesc[Statement], `block`: Suite, label: Option[Ident] = none(Ident), loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Loop, `block`: `block`, label: label, loc: loc)
proc LetSection*(_: typedesc[Statement], iddefSection: IdentDefSection, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.LetSection, iddefSection: iddefSection, loc: loc)
proc VarSection*(_: typedesc[Statement], iddefSection: IdentDefSection, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.VarSection, iddefSection: iddefSection, loc: loc)
proc ConstSection*(_: typedesc[Statement], iddefSection: IdentDefSection, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.ConstSection, iddefSection: iddefSection, loc: loc)
proc TypeSection*(_: typedesc[Statement], typedefSection: TypeDefSection, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.TypeSection, typedefSection: typedefSection, loc: loc)
proc Asign*(_: typedesc[Statement], pat: Pattern, op: Ident, val: Expression, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Asign, pat: pat, op: op, val: val, loc: loc)
proc IndexAssign*(_: typedesc[Statement], id: Ident, index: Expression, i_val: Expression, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.IndexAssign, id: id, index: index, i_val: i_val, loc: loc)
proc Funcdef*(_: typedesc[Statement], fn: Function, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Funcdef, fn: fn, loc: loc)
proc Meta*(_: typedesc[Statement], meta: Metadata, loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Meta, meta: meta, loc: loc)
proc Discard*(_: typedesc[Statement], dis: Option[Expression] = none(Expression), loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Discard, `discard`: dis, loc: loc)
proc Comments*(_: typedesc[Statement], comments: seq[Comment], loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Comments, comments: comments, loc: loc)
proc Expr*(_: typedesc[Statement], exp: Expression): Statement =
    Statement(kind: StatementKind.Expression, expression: exp, loc: exp.loc)
proc Fail*(_: typedesc[Statement], loc: Location = newLocation()): Statement =
    Statement(kind: StatementKind.Fail, loc: loc)

converter toStmt*(self: Expression): Statement =
    Statement.Expr(self)

proc empty*(_: typedesc[Statement]): Statement = Statement.Fail

proc get*(self: Program, line: int): Statement =
    for s in self.stmts:
        if line in s.loc.`range`.a.line..s.loc.`range`.b.line:
            return s
