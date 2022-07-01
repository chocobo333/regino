
import tables
import options

import il
import parsers
import sema
import desugar
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

proc getSource*(self: Project, uri: string=self.main): ref Source {.exportc: "get_s".} = 
    if not self.src.hasKey(uri):
        self.parse(uri)
    self.src[uri]

proc getProgram*(self: Project, uri: string=self.main): il.Program {.exportc: "get_p".} = 
    if not self.program.hasKey(uri):
        self.parse(uri)
    if not self.terrs.hasKey(uri):
        self.sema(uri)
    self.program[uri]

proc getTerrs*(self: Project, uri: string=self.main): seq[TypeError] {.exportc: "get_te".} = 
    if not self.program.hasKey(uri):
        self.parse(uri)
    if not self.terrs.hasKey(uri):
        self.sema(uri)
    self.terrs[uri]

proc getPerrs*(self: Project, uri: string=self.main): seq[ParseError] {.exportc: "get_pe".} = 
    if not self.perrs.hasKey(uri):
        self.parse(uri)
    self.perrs[uri]

proc parse*(self: Project, uri: string = self.main) {.exportc: "parse".} =
    let
        f = open(uri)
        text = f.readAll()
        src = Source.from(text, uri)
        program = Program(src).get.desugar
    close f
    self.src[uri] = src
    self.perrs[uri] = self.src[uri].errs
    self.program[uri] = program

proc sema*(self: Project, uri: string = self.main) {.exportc: "sema".} =
    let program = self.program[uri] 
    self.terrs[uri] = program.sema(self)

    
proc update*(self: Project, uri: string, text: string) =
    let
        src = Source.from(text, uri)
        program = Program(src).get.desugar
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
