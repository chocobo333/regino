
import tables
import sets
import options
import strformat
import strutils
import sequtils

import il
import relations
import orders
import errors


type
    Scope* = Table[string, seq[PSymbol]]
    Substitution* = Table[TypeVar, ref Type]
    TypeEnv* = object
        scopes*: seq[Scope]
        typeOrder*: seq[Order[ref Type]]
        constraints*: seq[Constraint]
        tvconstraints*: seq[Constraint]
        # subs*: Substitution
    Substituable* = concept T, var t
        apply(Substitution, T)
        t.ftv() is HashSet[TypeVar]
    Constraint* = (ref Type, ref Type)   # for t1 <= t2


proc newScope*(): Scope =
    initTable[string, seq[PSymbol]]()

proc pushScope*(env: var TypeEnv) =
    if env.scopes.len == 0:
        env.scopes.add newScope()
    # env.scopes.add env.scopes[^1]
    env.scopes.add newScope()
    if env.typeOrder.len == 0:
        env.typeOrder.add newOrder[ref Type]()
    env.typeOrder.add newOrder[ref Type]()

proc popScope*(env: var TypeEnv): (Scope, Order[ref Type]) {.discardable.} =
    (env.scopes.pop, env.typeOrder.pop)

proc `$`*(self: Scope): string =
    if self.len == 0:
        return "{}"
    for (key, val) in self.pairs:
        result &= &"\"{key}\": {val},\n"
    result = &"{{\n{result[0..^3].indent(2)}\n}}"

proc addIdent*(self: var TypeEnv, id: Ident, sym: PSymbol) =
    assert sym.ptyp.kind == PolyTypeKind.Forall

    let
        name = id.name
    if name in self.scopes[^1]:
        let kind = sym.kind
        if kind in {SymbolKind.Let, SymbolKind.Var, SymbolKind.Const}:
            self.scopes[^1][name] = self.scopes[^1][name].filterIt(it.kind notin {SymbolKind.Let, SymbolKind.Var, SymbolKind.Const})
        elif kind == SymbolKind.Typ:
            self.scopes[^1][name] = self.scopes[^1][name].filterIt(it.kind != SymbolKind.Typ)
        self.scopes[^1][name].add sym
    else:
        self.scopes[^1][name] = @[sym]

proc lookupId*(self: TypeEnv, name: string): seq[PSymbol] =
    var kinds: set[SymbolKind] = {}
    for i in 1..self.scopes.len:
        if name in self.scopes[^i]:
            result.add self.scopes[^i][name].filterIt(it.kind notin kinds)
            for k in self.scopes[^i][name].mapIt(it.kind):
                if k != SymbolKind.Func:
                    kinds.incl k

proc newTypeEnv*(): TypeEnv =
    TypeEnv(
        scopes: @[newScope()],
        typeOrder: @[newOrder[ref Type]()],
        # subs: initTable[TypeVar, ref Type]()
    )

let
    nullSubst* = initTable[TypeVar, ref Type]()
    nullFtv* = initHashSet[TypeVar]()

# TODO: Assert that `Type`, `PolyType` and so on are Substituable.
# assert

proc apply*(self: Substitution, t: ref Type): ref Type =
    case t.kind
    of TypeKind.Bottom..TypeKind.String:
        discard
    of TypeKind.List:
        t.elem = self.apply t.elem
    of TypeKind.Tuple, TypeKind.Intersection:
        for i in 0..<t.types.len:
            t.types[i] = self.apply t.types[i]
    of TypeKind.Record:
        for i in 0..<t.idtypes.len:
            t.idtypes[i] = (t.idtypes[i][0], self.apply t.idtypes[i][1])
    of TypeKind.Arrow:
        for i in 0..<t.paramty.len:
            t.paramty[i] = self.apply t.paramty[i]
        t.rety = self.apply t.rety
    of TypeKind.Var:
        if t.tv in self:
            # TODO: check overloads
            return self[t.tv]
        else:
            t.tv.ub = self.apply t.tv.ub
            t.tv.lb = self.apply t.tv.lb
    of TypeKind.Typedesc, TypeKind.Distinct:
        t.typ = self.apply(t.typ)
    of TypeKind.Link:
        t.to = self.apply(t.to)
    t

proc inst*(typ: PolyType): ref Type =
    result = case typ.kind
    of PolyTypeKind.ForAll:
        let
            gen = toSeq(typ.gen.items)
            s = gen.zip(gen.mapIt(Type.Var())).toTable
        s.apply(typ.typ)
    of PolyTypeKind.Intersection:
        Type.Intersection(typ.types.map(inst))

proc compose(a, b: Substitution): Substitution =
    result = initTable[TypeVar, ref Type]()
    for key, value in b:
        result[key] = a.apply(value)
    for key, value in a:
        if key notin result:
            result[key] = value
        else:
            if value.kind == TypeKind.Var:
                if value.tv notin result:
                    result[value.tv] = Type.Var(key)
                else:
                    raise newException(TypeError, "")
proc `@`*(a, b: Substitution): Substitution =
    compose(a, b)
