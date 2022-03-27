
import options

import il


proc newIdentdef*(pat: Pattern, typ: Option[Expression] = none(Expression), default: Option[Expression] = none(Expression)): IdentDef =
    IdentDef(pat: pat, typ: typ, default: default)
proc newTypedef*(id: Ident, params: Option[seq[GenTypeDef]], typ: TypeExpression): TypeDef =
    TypeDef(id: id, params: params, typ: typ)
proc newGenTypedef*(id: Ident, ub: Option[Expression] = none(Expression)): GenTypeDef =
    GenTypeDef(id: id, ub: ub)
