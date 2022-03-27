
import il

import values


var
    tvid = 0

proc newTypeVar*(): TypeVar =
    inc tvid
    TypeVar(
        id: tvid,
        ub: Value.Unit,
        lb: Value.Bottom
    )
