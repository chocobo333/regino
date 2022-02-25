
import il
import ../lineinfos


proc newIdent*(name: string, loc: Location = newLocation()): Ident =
    Ident(name: name, loc: loc)

converter toIdent*(self: string): Ident =
    newIdent(self)
