
import tables
import options

import il
import parsers
import desugar

type
    Buffer*[T] = Table[string, T]
    Project* = ref object
        main*: string
        program*: Buffer[il.Program]

proc newBuffer*[T](): Buffer[T] =
    initTable[string, T]()
proc newProject*(main: string): Project =
    result = Project(main: main)
    result.program = newBuffer[il.Program]()

proc buildProject*(main: string): Project =
    let 
        f = open(main)
        s = f.readAll
        program = Program(Source.from(s)).get.desugar
        project = newProject(main)
    
    project.program[main] = program
    project