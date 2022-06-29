
import strformat
import il
import lineinfos


type
    TypeErrorKind {.pure.} = enum
        Discard
        Subtype
        CircularSubtype
        Undeciable
        Undefined
        NoSuite
        Unasignable
        Letasign
        SomethingWrong
        Internal
        NotMatch
    TypeError* = object of CatchableError
        loc*: Location
        case kind: TypeErrorKind
        of TypeErrorKind.Discard:
            s*: Statement
        of TypeErrorKind.Subtype, TypeErrorKind.CircularSubtype, NotMatch:
            t1, t2: Value
        of TypeErrorKind.Undeciable:
            nil
        of TypeErrorKind.Undefined, TypeErrorKind.Letasign:
            id: Ident
        of TypeErrorKind.NoSuite:
            nil
        of TypeErrorKind.Unasignable:
            pat: Pattern
            val: Expression
        of TypeErrorKind.SomethingWrong, TypeErrorKind.Internal:
            stacktrace: string
            i_msg: string

proc `$`*(self: TypeError): string =
    case self.kind
    of TypeErrorKind.Discard:
        fmt"expression `{self.s}` is of type {self.s.typ}, and has to be used or discarded"
    of TypeErrorKind.Subtype:
        fmt"{self.t1} is not subtype of {self.t2}"
    of TypeErrorKind.CircularSubtype:
        fmt"{self.t1} and {self.t2} are subtypes of each other"
    of TypeErrorKind.Undeciable:
        fmt"type of this term is undiciable"
    of TypeErrorKind.Undefined:
        fmt"ident `{self.id}` is not defined"
    of TypeErrorKind.NoSuite:
        fmt"no block"
    of TypeErrorKind.Unasignable:
        fmt"`{self.val}` is not asignable to `{self.pat}`, because `{self.pat}` is of {self.pat.typ}"
    of TypeErrorKind.Letasign:
        fmt"`{self.id}` cannot be re-asigned to, because `{self.id}` was defined in let section"
    of TypeErrorKind.SomethingWrong:
        &"something is wrong\n{self.stacktrace}"
    of TypeErrorKind.Internal:
        &"{self.i_msg}\n{self.stacktrace}"
    of TypeErrorKind.NotMatch:
        fmt"Expected {self.t2}, but got {self.t1}"

proc new*(self: TypeError): ref TypeError =
    newException(TypeError, $self)

proc Discard*(_: typedesc[TypeError], s: Statement): TypeError =
    TypeError(kind: TypeErrorKind.Discard, s: s, loc: s.loc)
proc Subtype*(_: typedesc[TypeError], t1, t2: Value, loc: Location = newLocation()): TypeError =
    TypeError(kind: TypeErrorKind.Subtype, t1: t1, t2: t2, loc: loc)
proc CircularSubtype*(_: typedesc[TypeError], t1, t2: Value, loc: Location = newLocation()): TypeError =
    TypeError(kind: TypeErrorKind.CircularSubtype, t1: t1, t2: t2, loc: loc)
proc Undeciable*(_: typedesc[TypeError], loc: Location = newLocation()): TypeError =
    TypeError(kind: TypeErrorKind.Undeciable, loc: loc)
proc Undefined*(_: typedesc[TypeError], id: Ident, loc: Location = newLocation()): TypeError =
    TypeError(kind: TypeErrorKind.Undefined, id: id, loc: loc)
proc NoSuite*(_: typedesc[TypeError], loc: Location = newLocation()): TypeError =
    TypeError(kind: TypeErrorKind.NoSuite, loc: loc)
proc Unasignable*(_: typedesc[TypeError], pat: Pattern, val: Expression, loc: Location = newLocation()): TypeError =
    TypeError(kind: TypeErrorKind.Unasignable, pat: pat, val: val, loc: loc)
proc Letasign*(_: typedesc[TypeError], id: Ident, loc: Location = newLocation()): TypeError =
    TypeError(kind: TypeErrorKind.Letasign, id: id, loc: loc)
proc SomethingWrong*(_: typedesc[TypeError], loc: Location = newLocation(), stacktrace: string = getStackTrace()): TypeError =
    TypeError(kind: TypeErrorKind.SomethingWrong, loc: loc, stacktrace: stacktrace)
proc Internal*(_: typedesc[TypeError], msg: string, loc: Location = newLocation(), stacktrace: string = getStackTrace()): TypeError =
    TypeError(kind: TypeErrorKind.Internal, loc: loc, i_msg: msg, stacktrace: stacktrace)
proc NotMatch*(_: typedesc[TypeError], t1, t2: Value, loc: Location = newLocation()): TypeError =
    TypeError(kind: TypeErrorKind.NotMatch, t1: t1, t2: t2, loc: loc)
