
import macros
import tables
import sets
import sugar


macro suite*(label: untyped, body: untyped): untyped =
    body

template debug*(exp: untyped): untyped =
    debugEcho exp.astToStr, " : ", exp

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
