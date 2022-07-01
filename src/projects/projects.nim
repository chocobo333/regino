
import tables

import ../il
import ../parsers
import ../errors
# import codegen


type
    Buffer*[T] = TableRef[string, T]
    Project* = ref object
        main*: string
        src*: Buffer[ref Source]
        program*: Buffer[il.Program]
        terrs*: Buffer[seq[TypeError]]
        perrs*: Buffer[seq[ParseError]]
        
proc getSource*(self: Project, uri: string=self.main): ref Source {.importc: "get_s".}
proc getProgram*(self: Project, uri: string=self.main): il.Program {.importc: "get_p".}
proc getTerrs*(self: Project, uri: string=self.main): seq[TypeError] {.importc: "get_te".}
proc getPerrs*(self: Project, uri: string=self.main): seq[ParseError] {.importc: "get_pe".}

proc parse*(self: Project, uri: string = self.main) {.importc: "parse".}
proc sema*(self: Project, uri: string = self.main) {.importc: "sema".}