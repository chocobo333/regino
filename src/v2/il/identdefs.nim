
import options

import il


proc newIdentdef*(pat: Pattern, typ: Option[Expression] = none(Expression), default: Option[Expression] = none(Expression)): IdentDef =
    IdentDef(pat: pat, typ: typ, default: default)
