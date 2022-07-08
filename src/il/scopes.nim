
import tables
import sequtils

import il
import values
import ../orders
import ../lineinfos
import ../utils


iterator items*(self: Scope): Scope =
    ## The first element is the youngest scope
    var scope = self
    while not scope.isNil:
        yield scope
        for prog in scope.imports:
            yield prog.scope
        scope = scope.parent

iterator reversed*(self: Scope): Scope =
    ## The first element is the oldest scope
    for scope in toSeq(self.items).reversed:
        yield scope

proc lookupId*(self: Scope, name: string, kinds: set[SymbolKind] = {SymbolKind.low..SymbolKind.high}): seq[Symbol] =
    var
        tmp: set[SymbolKind]
        varsKind = {SymbolKind.Let, SymbolKind.Var, SymbolKind.Const, SymbolKind.Param}
        typesKind = {SymbolKind.Typ, SymbolKind.GenParam}
        funcsKind = {SymbolKind.Func, SymbolKind.Field, SymbolKind.Enum}
    for scope in self:
        if name in scope.syms:
            let
                syms = scope.syms[name].filterIt(it.kind notin tmp and it.kind in kinds)
                vars = syms.filterIt(it.kind in varsKind)
                types = syms.filterIt(it.kind in typesKind)
                funcs = syms.filterIt(it.kind in funcsKind)
            if vars.len > 0:
                result.add vars[^1]
                tmp.incl varsKind
            if types.len > 0:
                result.add types[^1]
                tmp.incl typesKind
            result.add funcs
proc lookupId*(self: Scope, name: string, pos: Position, kinds: set[SymbolKind] = {SymbolKind.low..SymbolKind.high}): seq[Symbol] =
    var
        tmp: set[SymbolKind]
        varsKind = {SymbolKind.Let, SymbolKind.Var, SymbolKind.Const, SymbolKind.Param}
        typesKind = {SymbolKind.Typ, SymbolKind.GenParam}
        funcsKind = {SymbolKind.Func, SymbolKind.Field, SymbolKind.Enum}
    for scope in self:
        if name in scope.syms:
            let
                syms = scope.syms[name].filterIt(it.kind notin tmp and it.kind in kinds and it.id.loc.range.b <= pos)
                vars = syms.filterIt(it.kind in varsKind)
                types = syms.filterIt(it.kind in typesKind)
                funcs = syms.filterIt(it.kind in funcsKind)
            if vars.len > 0:
                result.add vars[^1]
                tmp.incl varsKind
            if types.len > 0:
                result.add types[^1]
                tmp.incl typesKind
            result.add funcs

proc getConverters*(self: Scope): Table[(Value, Value), Ident] = 
    for scope in self:
        for (k, v) in scope.converters.pairs:
            if not result.contains(k):
                result[k] = v

proc newScope*(parent: Scope = nil): Scope =
    Scope(
        parent: parent,
        imports: @[],
        syms: initTable[string, seq[Symbol]](),
        consts: initTable[string, seq[Symbol]](),
        typeOrder: if parent.isNil: newOrder[Value]() else: parent.typeOrder,
        converters: initTable[(Value, Value), Ident]()
    )
