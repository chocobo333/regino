
import options

import macros


# proc map*[T, U](val: Option[T], callback: proc(a: T): U): Option[U] =
#     if val.isSome:
#         some val.get.callback
#     else:
#         none U

macro mapIt*[T](val: Option[T], itexpr: untyped): untyped =
    let it = ident"it"
    quote do:
        `val`.map(
            proc(`it`: auto): auto =
                `itexpr`
        )

macro filterIt*[T](val: Option[T], itexpr: untyped): untyped =
    let
        it = ident"it"
        fil = bindSym("filter", brForceOpen)
    quote do:
        `val`.`fil`(
            proc(`it`: auto): auto =
                `itexpr`
        )