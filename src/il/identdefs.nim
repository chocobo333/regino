
import options

import il


proc newIdentdef*(pat: Pattern, typ: Option[Expression] = none(Expression), default: Option[Expression] = none(Expression), docStr: Option[string] = none(string)): IdentDef =
    IdentDef(kind: DefKind.Def, pat: pat, typ: typ, default: default, docStr: docStr)
proc newIdentdef*(comment: string): IdentDef =
    IdentDef(kind: DefKind.Comment, comment: comment)
proc newTypedef*(id: Ident, params: Option[seq[GenTypeDef]], typ: TypeExpression, docStr: Option[string] = none(string)): TypeDef =
    TypeDef(kind: DefKind.Def, id: id, params: params, typ: typ, docStr: docStr)
proc newTypedef*(comment: string): TypeDef =
    TypeDef(kind: DefKind.Comment, comment: comment)
proc newGenTypedef*(id: Ident, ub: Option[Expression] = none(Expression)): GenTypeDef =
    GenTypeDef(id: id, ub: ub)

import ../lineinfos
proc loc*(self: IdentDef): Location =
    let b = block:
        if self.default.isSome:
            self.default.get.loc
        elif self.typ.isSome:
            self.typ.get.loc
        else:
            self.pat.loc

    self.pat.loc..b
proc loc*(self: TypeDef): Location =
    self.id.loc..self.typ.loc
proc loc*(self: GenTypeDef): Location =
    if self.ub.isSome:
        self.id.loc..self.ub.get.loc
    else:
        self.id.loc
