
import strutils
import sugar

import parsers
import utils


proc many0*[I, O, E](parser: Parser[I, O, E]): Parser[I, seq[O], E] =
    newParser(
        proc(src: I): Result[I, seq[O], E] =
            var
                res: seq[O] = @[]
                src = src
            while true:
                let tmp = parser.parse(src)
                if tmp.isErr:
                    return ok (src, res)
                if not parser.dropped:
                    res.add tmp.get[1]
                src = tmp.get[0]
            return ok (src, res),
        genGraph("Many0", parser)
    )

proc many1*[I, O, E](parser: Parser[I, O, E]): Parser[I, seq[O], E] =
    newParser(
        proc(src: I): Result[I, seq[O], E] =
            var
                res: seq[O] = @[]
                src = src
            let tmp = parser.parse(src)
            if tmp.isErr:
                return err(tmp.getErr)
            res.add tmp.get[1]
            src = tmp.get[0]
            while true:
                let tmp = parser.parse(src)
                if tmp.isErr:
                    return ok (src, res)
                if not parser.dropped:
                    res.add tmp.get[1]
                src = tmp.get[0]
            return ok (src, res),
        genGraph("Many1", parser)
    )

proc manyMN*[I, O, E](parser: Parser[I, O, E], min, max: int): Parser[I, O, E] {.error("notimplemented").} =
    discard

proc times*[I, O, E](parser: Parser[I, O, E], n: int): Parser[I, seq[O], E] =
    newParser(
        proc(src: I): Result[I, seq[O], E] =
            var
                res: seq[O] = @[]
                src = src
            for i in 0..<n:
                let tmp = parser.parse(src)
                if tmp.isErr:
                    return err tmp.getErr
                if not parser.dropped:
                    res.add tmp.get[1]
                src = tmp.get[0]
            return ok (src, res),
        genGraph("Times", parser, n)
    )

# TODO: make acc var
proc fold0*[I, O, E, T](parser: Parser[I, O, E], init: T, accumulator: (T, O) -> T): Parser[I, T, E] =
    newParser(
        proc(src: I): Result[I, T, E] =
            var
                res: T = init
                src = src
            while true:
                let tmp = parser.parse(src)
                if tmp.isErr:
                    return ok (src, res)
                if not parser.dropped:
                    res = res.accumulator(tmp.get[1])
                src = tmp.get[0]
            return ok (src, res),
        genGraph("Fold0", parser, ("$1: $2" % [$init, $T]), ("$1 -> $2" % [$(T, O), $T]))
    )