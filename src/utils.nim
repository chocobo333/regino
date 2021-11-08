
import macros


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
