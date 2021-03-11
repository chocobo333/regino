
import strformat
import options
import sugar

import macros


type
    RK = enum
        Ok
        Err
    Result*[I, O, E] = object
        case kind: RK
        of Ok:
            ok*: (I, O) ## (remained, parsed)
        of Err:
            err*: E
    ParseProc*[I, O, E] = proc(src: I): Result[I, O, E]
    ToString*[T] = proc(self: T): string
    Parser*[I, O, E] = ref object
        parse*: ParseProc[I, O, E]
        dropped*: bool
        toString*: string

proc `$`*[I, O, E](self: Result[I, O, E]): string =
    case self.kind
    of Ok:
        fmt"Ok{self.ok}"
    of Err:
        fmt"Err(""{self.err}"")"

proc `$`*[I, O, E](self: Parser[I, O, E]): string =
    self.toString

proc `==`*[I, O, E](self, other: Result[I, O, E]): bool =
    if self.kind == other.kind:
        if self.kind == Ok:
            self.ok == other.ok
        else:
            self.err == other.err
    else:
        false

proc overwrite*[I, O, E](self: Parser[I, O, E], s: string): Parser[I, O, E] =
    self.toString = s
    self

proc newParser*[I, O, E](parse: ParseProc[I, O, E], tos: string, dropped: bool = false): Parser[I, O, E] =
    Parser[I, O, E](parse: parse, dropped: dropped, toString: tos)
    # Parser[I, O, E](parse: parse, dropped: dropped)

proc ok*[I, O, E](typ: typedesc[Result[I, O, E]], val: (I, O)): Result[I, O, E] =
    Result[I, O, E](kind: Ok, ok: val)

template ok*(val: typed): untyped =
    ok(typeof(result), val)

proc err*[I, O, E](cls: typedesc[Result[I, O, E]], msg: E): Result[I, O, E] =
    Result[I, O, E](kind: Err, err: msg)

template err*(msg: typed): untyped =
    err(typeof(result), msg)

proc isOk*[I, O, E](val: Result[I, O, E]): bool =
    val.kind == Ok

proc isErr*[I, O, E](val: Result[I, O, E]): bool =
    val.kind == Err

proc get*[I, O, E](self: Result[I, O, E]): (I, O) =
    self.ok

proc getErr*[I, O, E](self: Result[I, O, E]): E =
    self.err

proc map*[T, I, O, E](val: Option[T], callback: proc(a: T): (I, O), msg: E): Result[I, O, E] =
    if val.isSome:
        ok typeof(result), val.get.callback
    else:
        err(typeof(result), msg)

macro mapIt*[T](val: Option[T], itexpr: untyped, msg: string): untyped =
    let it = ident"it"
    quote do:
        `val`.map(
            proc(`it`: auto): auto =
                `itexpr`,
            `msg`
        )

proc map*[I, O1, O2, E](self: Result[I, O1, E], callback: O1 -> O2): Result[I, O2, E] =
    if self.isOk:
        ok (self.ok[0], self.ok[1].callback)
    else:
        err self.err

proc map*[I, O1, O2, E](self: Result[I, O1, E], callback: ((I, O1)) -> (I, O2)): Result[I, O2, E] =
    if self.isOk:
        ok self.ok.callback
    else:
        err self.err

proc mapErr*[I, O, E1, E2](self: Result[I, O, E1], callback: E1 -> E2): Result[I, O, E2] =
    if self.isErr:
        err self.err.callback
    else:
        ok self.ok

proc unwrap*[I, O, E](self: Result[I, O, E]): O =
    if self.isOk:
        return self.ok[1]
    else:
        raise newException(ValueError, $self.err)