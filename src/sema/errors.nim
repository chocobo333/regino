
import ../lineinfos

type
    ErrorKind* {.pure.} = enum
        InternalError
        NotDeclared
        Redifinition
    Error* = ref object of CatchableError
        loc*: Location
        case kind*: ErrorKind
        of ErrorKind.InternalError:
            nil
        of ErrorKind.NotDeclared:
            nil
        of ErrorKind.Redifinition:
            nil

proc InternalError*(_: typedesc[Error]): Error =
    Error(kind: ErrorKind.InternalError)
proc NotDeclared*(_: typedesc[Error]): Error =
    Error(kind: ErrorKind.NotDeclared)
proc Redifinition*(_: typedesc[Error]): Error =
    Error(kind: ErrorKind.Redifinition)
