
import strformat
import strutils
import nre
import options
import sugar

import parsers

import ../../lineinfos


proc s*(str: string): proc (self: ref Source): Option[string] =
    proc parse(self: ref Source): Option[string] =
        if self.src.startsWith(str):
            self.take(str)
            some(str)
        else:
            none(string)
    parse

proc p*(pattern: string): proc (self: ref Source): Option[string] =
    proc parse(self: ref Source): Option[string] =
        let
            p = re(fmt"(*UTF8){pattern}")
            mat = self.src.match(p)
        if mat.isSome:
            let
                str = mat.get.match
            self.take(str)
            some(str)
        else:
            none(string)
    parse

proc sp*(min: Natural): proc (self: ref Source): Option[string] =
    if min == 0:
        p(fmt"[[:blank:]]*")
    else:
        p(fmt"[[:blank:]]{{{min},}}")

proc pos*(self: ref Source): Option[Position] =
    some newPosition(self.line, self.character)
proc `%`*[T](parser: proc(self: ref Source): Option[T]): proc(self: ref Source): Option[(T, Location)] =
    proc parse(self: ref Source): Option[(T, Location)] =
        let
            pos = self.pos.get
            res = self.parser()
            endpos = self.pos.get
            loc = newLocation(self.uri, pos, endpos)
        if res.isSome:
            some (res.get, loc)
        else:
            none((T, Location))
    parse
proc `&`*[T](parser: proc(self: ref Source): Option[T]): proc(self: ref Source): Option[T] =
    proc parse(self: ref Source): Option[T] =
        let
            tmp = self[]
            res = self.parser()
        self[] = tmp
        res
    parse

proc `!`*[T](parser: proc(self: ref Source): Option[T]): proc(self: ref Source): Option[T] =
    proc parse(self: ref Source): Option[T] =
        let
            tmp = self[]
            res = self.parser()
        self[] = tmp
        if res.isSome:
            none(T)
        else:
            when compiles(T.empty):
                some(T.empty)
            else:
                some(T.default)

    parse
proc `?`*[T](parser: proc(self: ref Source): Option[T]): proc(self: ref Source): Option[Option[T]] =
    proc parse(self: ref Source): Option[Option[T]] =
        let
            res = self.parser()
        if res.isSome:
            res.map(proc(it: T): Option[T] = some(it))
        else:
            some none(T)
    parse
proc `+`*[O1, O2](
    p1: proc(self: ref Source): Option[O1],
    p2: proc(self: ref Source): Option[O2]
    ): proc(self: ref Source): Option[(O1, O2)] =
    proc parse(self: ref Source): Option[(O1, O2)] =
        let
            tmp = self[]
            res1 = self.p1
        if res1.isSome:
            let
                res2 = self.p2
            if res2.isSome:
                some((res1.get, res2.get))
            else:
                self[] = tmp
                none((O1, O2))
        else:
            self[] = tmp
            none((O1, O2))
    parse
proc delimited*[O1, O2, O3](
    p1: proc(self: ref Source): Option[O1],
    p2: proc(self: ref Source): Option[O2],
    p3: proc(self: ref Source): Option[O3]
    ): proc(self: ref Source): Option[O2] =
    (p1 + p2 + p3) @ (it => it[0][1])

proc `^`*[O1, O2](
    p1: proc(self: ref Source): Option[O1],
    p2: proc(self: ref Source): Option[O2]
    ): proc(self: ref Source): Option[O1] =
    delimited(p2, p1, p2)

proc `@`*[O1, O2](
    parser: proc(self: ref Source): Option[O1],
    callback: proc(it: O1): O2,
    ): proc(self: ref Source): Option[O2] =
    proc parse(self: ref Source): Option[O2] =
        self.parser.map(callback)
    parse
proc `@@`*[O1, O2](
    parser: proc(self: ref Source): Option[O1],
    callback: proc(it: Option[O1]): Option[O2],
    ): proc(self: ref Source): Option[O2] =
    proc parse(self: ref Source): Option[O2] =
        self.parser.callback
    parse

proc alt*[T](parsers: seq[proc(self: ref Source): Option[T]]): proc(self: ref Source): Option[T] =
    proc parse(self: ref Source): Option[T] =
        let tmp = self[]
        for parser in parsers:
            let res = self.parser
            if res.isSome:
                return res
        self[] = tmp
        return none(T)
    parse
proc alt*[T](parsers: varargs[proc(self: ref Source): Option[T]]): proc(self: ref Source): Option[T] =
    alt(@parsers)

proc `>`*[T](p1, p2: proc(self: ref Source): Option[T]): proc(self: ref Source): Option[seq[T]] =
    (p1 + p2) @ (it => @[it[0], it[1]])
proc `>`*[T](p1: proc(self: ref Source): Option[seq[T]], p2: proc(self: ref Source): Option[T]): proc(self: ref Source): Option[seq[T]] =
    (p1 + p2) @ (it => it[0] & it[1])

proc preceded*[O1, O2](
    p1: proc(self: ref Source): Option[O1],
    p2: proc(self: ref Source): Option[O2]
    ): proc(self: ref Source): Option[O2] =
    (p1 + p2) @ (it => it[1])
proc terminated*[O1, O2](
    p1: proc(self: ref Source): Option[O1],
    p2: proc(self: ref Source): Option[O2]
    ): proc(self: ref Source): Option[O1] =
    (p1 + p2) @ (it => it[0])

proc success*(T: typedesc): proc(self: ref Source): Option[T] =
    proc parse(self: ref Source): Option[T] =
        when compiles(T.empty):
            some T.empty
        else:
            some T.default
    parse
proc success*[T](): proc(self: ref Source): Option[T] =
    proc parse(self: ref Source): Option[T] =
        some T.default
    parse

proc `+`*[T](parser: proc(self: ref Source): Option[T]): proc(self: ref Source): Option[seq[T]] =
    proc parse(self: ref Source): Option[seq[T]] =
        var
            res = self.parser
        if res.isNone:
            return none(seq[T])
        var resseq = @[res.get]
        while true:
            res = self.parser
            if res.isNone:
                return some resseq
            resseq.add res.get
    parse
proc `*`*[T](parser: proc(self: ref Source): Option[T]): proc(self: ref Source): Option[seq[T]] =
    alt(+parser, success(seq[T]))
proc `^+`*[O1, O2](
    p1: proc(self: ref Source): Option[O1],
    p2: proc(self: ref Source): Option[O2]
    ): proc(self: ref Source): Option[seq[O1]] =
    p1 + *preceded(p2, p1) @ (it => it[0] & it[1])
proc `^*`*[O1, O2](
    p1: proc(self: ref Source): Option[O1],
    p2: proc(self: ref Source): Option[O2]
    ): proc(self: ref Source): Option[seq[O1]] =
    alt(p1 ^+ p2, success(seq[O1]))

proc `@`*[T](parser: proc(self: ref Source): Option[T]): proc(self: ref Source): Option[T] =
    parser
