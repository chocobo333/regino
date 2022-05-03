
import strformat


type
    # TypeErrorKind = enum
    #     Undeciable
    #     CantUnify
    TypeError* = object of CatchableError
    SemaError* = object of CatchableError
    InternalError* = object of CatchableError
    ErrorKind = enum
        Type
        Sema
        Internal
    Error* = object of CatchableError
        case kind: ErrorKind
        of ErrorKind.Type:
            te: ref TypeError
        of ErrorKind.Sema:
            se: ref SemaError
        of ErrorKind.Internal:
            ie: ref InternalError

type
    FileInfo = tuple[filename: string, line: int, column: int]
proc fileinfoImple(callerInfo: FileInfo): string =
    fmt"{callerInfo.filename} : {callerInfo.line}"

template fileinfo*(): untyped =
    fileinfoImple(instantiationInfo())

proc `$`*(self: Error): string =
    case self.kind
    of ErrorKind.Type:
        $self.te[]
    of ErrorKind.Sema:
        $self.se[]
    of ErrorKind.Internal:
        $self.ie[]

template `raise`*(self: Error): untyped =
    case self.kind
    of ErrorKind.Type:
        raise self.te
    of ErrorKind.Sema:
        raise self.se
    of ErrorKind.Internal:
        raise self.ie


proc Undeciable*(_: typedesc[TypeError], msg: string = ""): Error =
    Error(kind: ErrorKind.Type, te: newException(TypeError, "Undeciable: " & msg))
proc Undefined*(_: typedesc[SemaError], id: string): Error =
    Error(kind: ErrorKind.Sema, se: newException(SemaError, fmt"Undefined variable: {id}"))
proc new*(_: typedesc[InternalError]): Error =
    Error(kind: ErrorKind.Internal, ie: newException(InternalError, fileinfo()))
