
import macros
import tables
import sets
import sugar
import strformat


macro suite*(label: untyped, body: untyped): untyped =
    body

template debug*(exp: untyped): untyped =
    when defined(release):
        discard
    else:
        let
            info = instantiationInfo()
        debugEcho info.filename, "(", info.line, ", ", info.column, ") ", exp.astToStr, " : ", exp


iterator reversed*[T](s: seq[T]): T =
    for i in countdown(s.len-1, 0):
        yield s[i]
proc flatten*[T](s: seq[seq[T]]): seq[T] =
    for e in s:
        result.add e

proc map*[T, U, V](self: Table[T, U], f: U -> V): Table[T, V] =
    result = initTable[T, V]()
    for (key, val) in self.pairs:
        result[key] = f(val)
proc filter*[T, U](self: Table[T, U], f: U -> bool): Table[T, U] =
    result = initTable[T, U]()
    for (key, val) in self.pairs:
        if f(val):
            result[key] = val

proc filter*[T](self: HashSet[T], f: T -> bool): HashSet[T] =
    result = initHashSet[T]()
    for e in self:
        if f(e):
            result.incl(e)
proc any*[T](self: HashSet[T], f: T -> bool): bool =
    for e in self:
        if f(e):
            return true
    false
proc all*[T](self: HashSet[T], f: T -> bool): bool =
    for e in self:
        if not f(e):
            return false
    true

when defined(release):
    import strutils
    const wordSizeImpl = staticExec("getconf LONG_BIT").parseInt.uint
else:
    const wordSizeImpl = 64
proc wordSize*(): uint =
    wordSizeImpl

# suite Option:
#     type
#         Option*[T] = object
#             case has: bool
#             of true:
#                 val: T
#             of false:
#                 nil
#         UnpackDefect* = object of Defect

#     proc some*[T](val: T): Option[T] =
#         Option[T](
#             has: true,
#             val: val
#         )
#     proc none*(T: typedesc): Option[T] =
#         Option[T](has: false)
#     proc none*[T](): Option[T] =
#         none(T)
#     proc isSome*[T](self: Option[T]): bool {.inline.} =
#         self.has
#     proc isNone*[T](self: Option[T]): bool {.inline.} =
#         not self.has
#     proc get*[T](self: Option[T]): lent T {.inline.} =
#         if self.isNone:
#             raise newException(UnpackDefect, "Can't obtain a value from a `none`")
#         self.val
#     proc get*[T](self: Option[T], otherwise: T): T {.inline.} =
#         if self.isSome:
#             self.val
#         else:
#             otherwise

#     proc get*[T](self: var Option[T]): var T {.inline.} =
#         if self.isNone:
#             raise newException(UnpackDefect, "Can't obtain a value from a `none`")
#         self.val
#     proc map*[T, U](self: Option[T], f: proc (it: T): U): Option[U] {.inline.} =
#         if self.isSome:
#             some[U](f(self.val))
#         else:
#             none(U)
#     proc filter*[T](self: Option[T], f: proc (input: T): bool): Option[T] {.inline.} =
#         if self.isSome and not f(self.val):
#             none(T)
#         else:
#             self

#     proc `==`*(a, b: Option): bool {.inline.} =
#         (a.isSome and b.isSome and a.val == b.val) or (not a.isSome and not b.isSome)

#     proc `$`*[T](self: Option[T]): string =
#         if self.isSome:
#             result = "Some("
#             result.addQuoted self.val
#             result.add ")"
#         else:
#             result = "None[" & $T & "]"
#     import options
#     converter to*[T](self: options.Option[T]): utils.Option[T] =
#         if self.isSome:
#             utils.some (self.get)
#         else:
#             utils.none(T)
