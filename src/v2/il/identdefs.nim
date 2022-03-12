
import options

import il


proc newIdentdef*(pat: Pattern, typ: Option[Expression] = none(Expression), default: Option[Expression] = none(Expression)): IdentDef =
    IdentDef(pat: pat, typ: typ, default: default)
proc newTypedef*(id: Ident, params: Option[seq[IdentDef]], typ: TypeExpression): TypeDef =
    TypeDef(id: id, params: params, typ: typ)
