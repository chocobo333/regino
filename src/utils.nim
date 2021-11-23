
import macros
import tables
import sequtils


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

template mapIt*[T, U](self: Table[T, U], f: untyped): untyped =
    toSeq(self.pairs).mapIt((it[0], f)).toTable
