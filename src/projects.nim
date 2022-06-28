
import tables
import options

import il
import parsers
import sema
import errors
import projects/projects
import utils

export Project

proc newBuffer[T](): Buffer[T] =
    newTable[string, T]()

proc newProject*(main: string = ""): Project =
    Project(
        main: main,
        src: newBuffer[ref Source](),
        program: newBuffer[il.Program](),
        terrs: newBuffer[seq[TypeError]](),
        perrs: newBuffer[seq[ParseError]]()
    )

proc parse*(self: Project, uri: string = self.main) =
    let
        f = open(uri)
        text = f.readAll()
        src = Source.from(text, uri)
        program = Program(src).get
    close f
    self.src[uri] = src
    self.perrs[uri] = self.src[uri].errs
    self.program[uri] = program
proc sema*(self: Project, uri: string = self.main) =
    let
        program = self.program[uri]
    self.terrs[uri] = program.sema(self)

    
proc update*(self: Project, uri: string, text: string) =
    let
        src = Source.from(text, uri)
        program = Program(src).get
    self.terrs[uri] = program.sema(self)
    self.src[uri] = src
    self.perrs[uri] = self.src[uri].errs
    self.program[uri] = program

proc `[]=`*(self: Project, uri: string, text: string) =
    self.update(uri, text)

proc mainProgram*(self: Project): Program =
    self.program[self.main]

proc errExists*(self: Project): bool =
    self.perrs.len != 0 or self.terrs.len != 0

proc echoErrs*(self: Project) =
    for (key, val) in self.perrs.pairs:
        debug key
        debug val
    for (key, val) in self.terrs.pairs:
        debug key
        debug val
