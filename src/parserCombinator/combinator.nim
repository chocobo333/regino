
import strutils
import sequtils
import options
import strformat
import sugar

import parsers
import nones
import repeats
import utils

import macros


proc alt*[I, O, E](parsers: seq[Parser[I, O, E]]): Parser[I, O, E] =
    newParser(
        proc (src: I): Result[I, O, E] =
            for parser in parsers:
                result = parser.parse(src)
                if result.isOk:
                    return,
        genGraphS("Alt", parsers)
    )

proc alt*[I, O, E](parsers: varargs[Parser[I, O, E]]): Parser[I, O, E] =
    alt(@parsers)

proc tupl*[I, O1, O2, E](a: Parser[I, O1, E], b: Parser[I, O2, E]): Parser[I, (O1, O2), E] =
    type
        O = (O1, O2)
    result = newParser(
        proc(src: I): Result[I, O, E] =
            let resultA = a.parse(src)
            if resultA.isOk:
                let
                    resultA = resultA.get
                    resultB = b.parse(resultA[0])
                if resultB.isOk:
                    let
                        resultB = resultB.get
                    return ok (resultB[0], (resultA[1], resultB[1]))
                else:
                    return err(resultB.getErr)
            else:
                return err(resultA.getErr),
        genGraph("Tuple", a, b)
    )

proc tupl*[I, O1, O2, O3, E](tup: (Parser[I, O1, E], Parser[I, O2, E], Parser[I, O3, E])): Parser[I, (O1, O2, O3), E] =
    newParser(
        proc(src: I): Result[I, (O1, O2, O3), E] =
            var
                res: (O1, O2, O3)
                src = src
            let tmp0 = tup[0].parse(src)
            if tmp0.isErr:
                return err(tmp0.getErr)
            res[0] = tmp0.get[1]
            src = tmp0.get[0]
            
            let tmp1 = tup[1].parse(src)
            if tmp1.isErr:
                return err(tmp1.getErr)
            res[1] = tmp1.get[1]
            src = tmp1.get[0]

            let tmp2 = tup[2].parse(src)
            if tmp2.isErr:
                return err(tmp2.getErr)
            res[2] = tmp0.get[1]
            src = tmp2.get[0]
            return ok (src, res),
        genGraph("Tuple", tup[0], tup[1], tup[2])
    )

proc sequence*[I, O, E](parsers: seq[Parser[I, O, E]]): Parser[I, seq[O], E] =
    newParser(
        proc(src: I): Result[I, seq[O], E] =
            var
                res: seq[O] = @[]
                src = src
            for parser in parsers:
                let tmp = parser.parse(src)
                if tmp.isErr:
                    return err(tmp.getErr)
                if not parser.dropped:
                    res.add tmp.get[1]
                src = tmp.get[0]
            return ok (src, res),
        genGraphS("Sequence", parsers)
    )

proc sequence*[I, O, E](parsers: varargs[Parser[I, O, E]]): Parser[I, seq[O], E] =
    sequence(@parsers)

# proc array*[I, O, E, N](parsers: array[N, Parser[I, O, E]]): Parser[I, array[N, O], E] =
#     newParser(
#         proc(src: I): Result[I, array[N, O], E] =
#             var
#                 res: array[N, O]
#                 src = src
#             for i, parser in parsers:
#                 let tmp = parser.parse(src)
#                 if tmp.isErr:
#                     return err(tmp.getErr)
#                 res[i] = tmp.get[1]
#                 src = tmp.get[0]
#             return ok (src, res)
#     )

proc map*[I, O1, O2, E](parser: Parser[I, O1, E], callback: O1 -> O2): Parser[I, O2, E] =
    newParser(
        proc(src: I): Result[I, O2, E] =
            parser.parse(src).map(callback),
        genGraph("Map", parser, ("$1 -> $2" % [$O1, $O2]))
    )

proc mapRes*[I, O1, O2, E](parser: Parser[I, O1, E], callback: Result[I, O1, E] -> Result[I, O2, E]): Parser[I, O2, E] =
    newParser(
        proc(src: I): Result[I, O2, E] =
            parser.parse(src).callback,
        genGraph("MapRes", parser, ("$1 -> $2" % [$Result[I, O1, E], $Result[I, O2, E]]))
    )

proc mapErr*[I, O, E1, E2](parser: Parser[I, O, E1], callback: E1 -> E2): Parser[I, O, E2] =
    newParser(
        proc(src: I): Result[I, O, E2] =
            parser.parse(src).mapErr(callback),
        genGraph("MapErr", parser, ("$1 -> $2" % [$E1, $E2]))
    )

proc opt*[I, O, E](parser: Parser[I, O, E]): Parser[I, Option[O], E] =
    newParser(
        proc(src: I): Result[I, Option[O], E] =
            let tmp = parser.parse(src)
            if tmp.isOk:
                let tmp = tmp.get
                ok (tmp[0], some tmp[1])
            else:
                ok (src, none(O))
        ,
        genGraph("Opt", parser)
    )
    
proc flatten[T](self: seq[seq[T]]): seq[T] =
    result = @[]
    for e in self:
        result.add e

proc separated1*[I, O1, O2, E](parser: Parser[I, O1, E], delimiter: Parser[I, O2, E]): Parser[I, seq[O1], E] =
    let tmp = tupl(delimiter, parser).map(it => it[1])
    tupl(parser, many0(tmp)).map(it => it[0] & it[1]).overwrite(
        genGraph("Separated1", parser, delimiter)
    )

proc separated0*[I, O1, O2, E](parser: Parser[I, O1, E], delimiter: Parser[I, O2, E]): Parser[I, seq[O1], E] =
    alt(separated1(parser, delimiter), nones.none[I, seq[O1], E]()).overwrite(
        genGraph("Separated0", parser, delimiter)
    )

proc terminated*[I, O1, O2, E](parser: Parser[I, O1, E], terminator: Parser[I, O2, E]): Parser[I, O1, E] =
    # newParser(
    #     proc(src: I): Result[I, O1, E] =
    #         result = parser.parse(src)
    #         if result.isOk:
    #             let tmp = terminator.parse(result.get[0])
    #             if tmp.isOk:
    #                 return ok (tmp.get[0], result.get[1])
    #             else:
    #                 return err tmp.getErr
    #         else:
    #             return
    # )
    tupl(parser, terminator).map(it=>it[0]).overwrite(
        genGraph("Terminated", parser, terminator)
    )
proc preceded*[I, O1, O2, E](precedor: Parser[I, O1, E], parser: Parser[I, O2, E]): Parser[I, O2, E] =
    # tupl(la(precedor), tupl(precedor, parser)).map(it=>it[1][1])
    tupl(precedor, parser).map(it=>it[1]).overwrite(
        genGraph("Preceded", precedor, parser)
    )

proc delimited*[I, O1, O2, O3, E](a: Parser[I, O1, E], b: Parser[I, O2, E], c: Parser[I, O3, E]): Parser[I, O2, E] =
    tupl(tupl(a, b), c).map(it=>it[0][1]).overwrite(
        genGraph("Delimited", a, b, c)
    )

proc surrounded*[I, O1, O2, E](outer: Parser[I, O1, E], inner: Parser[I, O2, E]): Parser[I, O2, E] =
    delimited(outer, inner, outer).overwrite(
        genGraph("Surronded", outer, inner)
    )

when isMainModule:
    import patterns
    import strutils

    let
        a = pattern[string]"a*"
        b = pattern[string]"b*"
    echo alt(@[a, b])
    echo alt(@[a, b]).parse("aaabbb")
    echo alt(a, b).parse("aaabbb")
    echo alt(a, b).parse("bbbaaabbb")
    echo tupl(b, a)
    echo tupl(b, a).parse("bbbaaabbb")
    echo sequence(@[a, b])
    echo sequence(@[a, b]).parse("bbbaaabbb")
    echo sequence(b, a, b)
    echo sequence(b, a, b).parse("bbbaaabbb")
    echo many0(str[string]"b").parse("bbbbb")
    echo many0(str[string]"a").parse("bbbbb")
    echo many1(str[string]"a").parse("bbbbb")
    echo many1(str[string]"a")
    echo many1(str[string]"b").parse("bbbbb")
    echo sequence(a, b).map(it => it.join)
    echo sequence(a, b).map(it => it.join).parse("aabbaa")
    echo opt(str[string]"+")
    echo opt(str[string]"+").parse("3")
    echo opt(str[string]"+").parse("+3")
    echo separated1(str[string]"a", str[string](","))
    echo separated1(str[string]"a", str[string](",")).parse("a,a,a,a")
    echo separated1(str[string]"b", str[string](",")).parse("a,a,a,a")
    echo separated0(str[string]"b", str[string](","))
    echo separated0(str[string]"b", str[string](",")).parse("a,a,a,a")
    echo separated0(str[string]"a", str[string](",")).parse("a,a,a,a")
    echo terminated(pattern[string]"[0-9]+", str[string]";")
    echo terminated(pattern[string]"[0-9]+", str[string]";").parse("123;")
    echo terminated(pattern[string]"[0-9]+", str[string]";").parse("123:")
    echo terminated(pattern[string]"[0-9]+", str[string]";").parse("w123:")
    echo preceded(pattern[string]"[0-9]+", str[string]";")
    echo preceded(pattern[string]"[0-9]+", str[string]";").parse("123;")
    echo preceded(pattern[string]"[0-9]+", str[string]";").parse("abc;")
    echo preceded(pattern[string]"[0-9]+", str[string]";").parse("123:")
    echo delimited(str[string]"(", separated1(pattern[string]"\\w", str[string](",")), str[string]")")
    echo delimited(str[string]"(", separated1(pattern[string]"\\w", str[string](",")), str[string]")").parse("(a,b,c)")
    let parser1 = pattern[string](".").mapRes(
        proc(it: Result[string, string, string]): Result[string, string, string] =
            result = it
            if it.isOk and it.get[1] == "a":
                result = err "a!"
    )
    echo parser1
    echo parser1.parse("a")
    echo parser1.parse("b")
    # echo array([b, a, b]).parse("bbbaaabbb")
