
import tables

import il

type
    Buffer*[T] = Table[string, T]
    Project* = ref object
        main*: string
        program*: Buffer[Program]

proc newBuffer*[T](): Buffer[T] =
    initTable[string, T]()
proc newProject*(main: string): Project =
    result = Project(main: main)
    result.program = newBuffer[Program]()