
import tables
import options

import ir
import typeenvs
import errors


type
    Buffer*[T] = (Project, Table[string, T])
    Project* = ref object
        main*: string
        programs*: Buffer[Expression]
        errs*: seq[Error]
        env*: TypeEnv


proc newBuffer*[T](p: Project): Buffer[T] =
    (p, initTable[string, T]())
proc newProject*(main: string): Project =
    result = Project(main: main)
    result.programs = newBuffer[Expression](result)

proc addErr*(self: Project, err: Option[Error]) =
    if err.isSome:
        self.errs.add err.get
