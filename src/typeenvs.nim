
import tables
import sets
import sequtils

import il
import errors


type
    Substitution* = Table[TypeVar, ref Type]
    TypeEnv* = object
        scope*: Scope
        constraints*: seq[Constraint]
        tvconstraints*: seq[Constraint]
    Substituable* = concept T, var t
        apply(Substitution, T)
        t.ftv() is HashSet[TypeVar]
    Constraint* = (ref Type, ref Type)   # for t1 <= t2


proc pushScope*(env: var TypeEnv, scope: Scope) =
    assert scope.parent == env.scope
    env.scope = scope

proc popScope*(env: var TypeEnv): Scope {.discardable.} =
    result = env.scope
    env.scope = env.scope.parent

proc lookupId*(self: TypeEnv, name: string, kinds: set[SymbolKind] = {SymbolKind.low..SymbolKind.high}): seq[PSymbol] =
    var
        tmp: set[SymbolKind]
    for scope in self.scope:
        if name in scope.syms:
            let
                syms = scope.syms[name].filterIt(it.kind notin tmp and it.kind in kinds)
                vars = syms.filterIt(it.kind in {SymbolKind.Let, SymbolKind.Var, SymbolKind.Const})
                types = syms.filterIt(it.kind == SymbolKind.Typ)
                funcs = syms.filterIt(it.kind == SymbolKind.Func)
            if vars.len > 0:
                result.add vars[^1]
            if types.len > 0:
                result.add types[^1]
            result.add funcs
            for k in syms.mapIt(it.kind):
                if k != SymbolKind.Func:
                    tmp.incl k

proc lookupConverter*(self: Scope, t1, t2: ref Type): Ident =
    for scope in self:
        if (t1, t2) in scope.converters:
            return scope.converters[(t1, t2)]
    raise newException(TypeError, "")
proc lookupConverter*(self: TypeEnv, t1, t2: ref Type): Ident =
    self.scope.lookupConverter(t1, t2)

proc addIdent*(self: var TypeEnv, id: Ident, sym: PSymbol) =
    assert sym.ptyp.kind == PolyTypeKind.Forall

    let
        name = id.name
    if name in self.scope.syms:
        # let kind = sym.kind
        # if kind in {SymbolKind.Let, SymbolKind.Var, SymbolKind.Const}:
        #     self.scope.syms[name] = self.scope.syms[name].filterIt(it.kind notin {SymbolKind.Let, SymbolKind.Var, SymbolKind.Const})
        # elif kind == SymbolKind.Typ:
        #     self.scope.syms[name] = self.scope.syms[name].filterIt(it.kind != SymbolKind.Typ)
        self.scope.syms[name].add sym
    else:
        self.scope.syms[name] = @[sym]

proc newTypeEnv*(scope: Scope): TypeEnv =
    TypeEnv(
        scope: scope
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
    of TypeKind.Pair:
        t.first = self.apply t.first
        t.second = self.apply t.second
    # of TypeKind.Tuple, TypeKind.Intersection:
    of TypeKind.Intersection:
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
