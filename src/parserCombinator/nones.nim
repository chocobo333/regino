
import parsers
import concepts


proc empty*(typ: typedesc[string]): string =
    ""
proc empty*[T](typ: typedesc[seq[T]]): seq[T] =
    @[]

proc none*[I, O, E: Emptiable](): Parser[I, O, E] =
    newParser(
        proc(src: I): Result[I, O, E] =
            ok(typeof(result), (src, O.empty)),
        "none"
    )


when isMainModule:
    assert string is Emptiable
    assert seq[string] is Emptiable