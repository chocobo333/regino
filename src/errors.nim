
import strformat


type
    # TypeErrorKind = enum
    #     Undeciable
    #     CantUnify
    TypeError* = object of CatchableError
    InternalError* = object of CatchableError
    ErrorKind = enum
        Type
        Internal
    Error* = object of CatchableError
        case kind: ErrorKind
        of ErrorKind.Type:
            te: ref TypeError
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
    of ErrorKind.Internal:
        $self.ie[]

proc Undeciable*(_: typedesc[TypeError]): Error =
    Error(kind: ErrorKind.Type, te: newException(TypeError, "Undeciable"))
proc new*(_: typedesc[InternalError]): Error =
    Error(kind: ErrorKind.Internal, ie: newException(InternalError, fileinfo()))
