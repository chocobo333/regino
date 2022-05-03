
import il

import values


var
    tvid = 0

proc getTvid*(): int =
    inc tvid
    tvid
proc newTypeVar*(): TypeVar =
    inc tvid
    TypeVar(
        id: tvid,
        ub: Value.Unit,
        lb: Value.Bottom
    )

proc newGenericType*(ident: Ident, ub: Value, typ: Value): GenericType =
    inc tvid
    GenericType(
        id: tvid,
        ident: ident,
        ub: ub,
        typ: typ
    )
