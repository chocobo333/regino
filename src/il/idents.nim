
import hashes

import il
import ../lineinfos


proc newIdent*(name: string, loc: Location = newLocation()): Ident =
    Ident(name: name, loc: loc)

converter toIdent*(self: string): Ident =
    newIdent(self)

proc empty*(_: typedesc[Ident]): Ident =
    ""

proc hash*(self: Ident): Hash =
    self.name.hash

proc `==`*(self, other: Ident): bool =
    self.name == other.name
when isMainModule:
    echo newIdent("a") == newIdent("a")
