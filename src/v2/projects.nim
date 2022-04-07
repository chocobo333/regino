
import tables
import options

import il
import parsers
import sema
import errors
# import codegen


type
    Buffer[T] = Table[string, T]
    Project* = ref object
        src*: Buffer[ref Source]
        program*: Buffer[il.Program]
        terrs*: Buffer[seq[TypeError]]

proc newBuffer[T](): Buffer[T] =
    initTable[string, T]()

proc newProject*(): Project =
    Project(
        src: newBuffer[ref Source](),
        program: newBuffer[il.Program](),
    )

proc update*(self: Project, uri: string, text: string) =
    let
        src = Source.from(text, uri)
        program = Program(src).get
    self.terrs[uri] = program.sema
    self.src[uri] = src
    self.program[uri] = program

proc `[]=`*(self: Project, uri: string, text: string) =
    self.update(uri, text)

proc perrs*(self: Project, uri: string): seq[ParseError] =
    self.src[uri].errs
