
import ../lineinfos

type
    ErrorKind* {.pure.} = enum
        InternalError
        NotDeclared
    Error* = ref object of CatchableError
        loc*: Location
        case kind*: ErrorKind
        of ErrorKind.InternalError:
            nil
        of ErrorKind.NotDeclared:
            nil

proc InternalError*(_: typedesc[Error]): Error =
    Error(kind: ErrorKind.InternalError)
proc NotDeclared*(_: typedesc[Error]): Error =
    Error(kind: ErrorKind.NotDeclared)
