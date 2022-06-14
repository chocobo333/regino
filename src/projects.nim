
import tables
import options

import il
import parsers
import sema
import errors
import projects/projects

export Project

proc newBuffer[T](): Buffer[T] =
    initTable[string, T]()

proc newProject*(main: string = ""): Project =
    Project(
        main: main,
        src: newBuffer[ref Source](),
        program: newBuffer[il.Program](),
    )

proc parse*(self: Project): Program =
    let
        uri = self.main
        f = open(uri)
        text = f.readAll()
        src = Source.from(text, uri)
        program = Program(src).get
    close f
    self.Terrs[uri] = program.sema(self)
    self.src[uri] = src
    self.program[uri] = program
    program
proc update*(self: Project, uri: string, text: string) =
    let
        src = Source.from(text, uri)
        program = Program(src).get
    self.Terrs[uri] = program.sema(self)
    self.src[uri] = src
    self.program[uri] = program

proc `[]=`*(self: Project, uri: string, text: string) =
    self.update(uri, text)

proc perrs*(self: Project, uri: string): seq[ParseError] =
    self.src[uri].errs

proc program*(self: Project): Program =
    self.program[self.main]
proc perrs*(self: Project): seq[ParseError] =
    self.perrs(self.main)
proc terrs*(self: Project): seq[TypeError] =
    self.Terrs[self.main]
proc terrs*(self: Project, uri: string): seq[TypeError] =
    self.Terrs[uri]
