
import tables
import sequtils

import il
import ../orders
import ../utils


iterator items*(self: Scope): Scope =
    ## The first element is the youngest scope
    var scope = self
    while not scope.isNil:
        yield scope
        scope = scope.parent

iterator reversed*(self: Scope): Scope =
    ## The first element is the oldest scope
    for scope in toSeq(self.items).reversed:
        yield scope

proc newScope*(parent: Scope = nil): Scope =
        Scope(
            parent: parent,
            syms: initTable[string, seq[Symbol]](),
            consts: initTable[string, seq[Symbol]](),
            typeOrder: if parent.isNil: newOrder[Value]() else: parent.typeOrder,
            converters: initTable[(Value, Value), Ident]()
        )
