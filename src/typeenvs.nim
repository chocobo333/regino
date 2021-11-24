
import tables
import sets
import sequtils
import hashes

import il
import errors


type
    Substitution* = Table[TypeVar, ref Value]
    TypeEnv* = object
        scope*: Scope
        constraints*: seq[Constraint]
        tvconstraints*: seq[Constraint]
    Substituable* = concept T, var t
        apply(Substitution, T)
        t.ftv() is HashSet[TypeVar]
    Constraint* = (ref Value, ref Value)   # for t1 <= t2


proc hash*(self: Constraint): Hash =
    result = self[0].hash
    result = result !& self[1].hash
    result = !$result

proc pushScope*(env: var TypeEnv, scope: Scope) =
    assert scope.parent == env.scope
    env.scope = scope

proc popScope*(env: var TypeEnv): Scope {.discardable.} =
    result = env.scope
    env.scope = env.scope.parent

proc lookupId*(self: TypeEnv, name: string, kinds: set[SymbolKind] = {SymbolKind.low..SymbolKind.high}): seq[Symbol] =
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
proc lookupConst*(self: TypeEnv, name: string): seq[Symbol] =
    for scope in self.scope:
        if name in scope.consts:
            result.add scope.consts[name]

proc lookupConverter*(self: Scope, t1, t2: ref Value): Ident =
    for scope in self:
        if (t1, t2) in scope.converters:
            return scope.converters[(t1, t2)]
    raise newException(TypeError, "")
proc lookupConverter*(self: TypeEnv, t1, t2: ref Value): Ident =
    self.scope.lookupConverter(t1, t2)

proc addIdent*(self: var TypeEnv, id: Ident, sym: Symbol) =
    let
        name = id.name
    if name in self.scope.syms:
        self.scope.syms[name].add sym
    else:
        self.scope.syms[name] = @[sym]
proc addConst*(self: var TypeEnv, id: Ident, sym: Symbol) =
    assert sym.kind in {SymbolKind.Const, SymbolKind.Typ}
    let
        name = id.name
    if name in self.scope.consts:
        self.scope.consts[name].add sym
    else:
        self.scope.consts[name] = @[sym]

proc newTypeEnv*(scope: Scope): TypeEnv =
    TypeEnv(
        scope: scope
    )

let
    nullSubst* = initTable[TypeVar, ref Value]()
    nullFtv* = initHashSet[TypeVar]()

# TODO: Assert that `Type`, `PolyType` and so on are Substituable.
# assert

proc apply*(self: Substitution, t: ref Value): ref Value =
    case t.kind
    of ValueKind.Bottom..ValueKind.String:
        discard
    # of ValueKind.List:
    #     t.elem = self.apply t.elem
    of ValueKind.Pair:
        t.first = self.apply t.first
        t.second = self.apply t.second
    of ValueKind.Record:
        for key in t.members.keys:
            t.members[key] = self.apply t.members[key]
    # of ValueKind.Tuple, ValueKind.Intersection:
    of ValueKind.Intersection:
        for i in 0..<t.types.len:
            t.types[i] = self.apply t.types[i]
    # of ValueKind.Record:
    #     for i in 0..<t.idtypes.len:
    #         t.idtypes[i] = (t.idtypes[i][0], self.apply t.idtypes[i][1])
    # of ValueKind.Arrow:
    #     for i in 0..<t.paramty.len:
    #         t.paramty[i] = self.apply t.paramty[i]
    #     t.rety = self.apply t.rety
    of ValueKind.Pi:
        for i in 0..<t.genty.len:
            t.genty[i][1] = self.apply t.genty[i][1]
        for i in 0..<t.paramty.len:
            t.paramty[i] = self.apply t.paramty[i]
        t.rety = self.apply t.rety
    # of ValueKind.Sigma:
    #     t.first = self.apply t.first
    #     t.second = self.apply t.second
    of ValueKind.Var:
        if t.tv in self:
            # TODO: check overloads
            return self[t.tv]
        else:
            t.tv.ub = self.apply t.tv.ub
            t.tv.lb = self.apply t.tv.lb
    # of ValueKind.Typedesc, ValueKind.Distinct:
    of ValueKind.Typedesc:
        t.`typedesc` = self.apply(t.`typedesc`)
    of ValueKind.Link:
        t.to = self.apply(t.to)
    of ValueKind.Neutral:
        discard
    t

# proc inst*(typ: PolyType): ref Value =
#     result = case typ.kind
#     of PolyTypeKind.ForAll:
#         let
#             gen = toSeq(typ.gen.items)
#             s = gen.zip(gen.mapIt(Value.Var())).toTable
#         s.apply(typ.typ)
#     of PolyTypeKind.Intersection:
#         Value.Intersection(typ.types.map(inst))
proc inst*(typ: ref Value): ref Value =
    # result = case typ.kind
    # of PolyTypeKind.ForAll:
    #     let
    #         gen = toSeq(typ.gen.items)
    #         s = gen.zip(gen.mapIt(Value.Var())).toTable
    #     s.apply(typ.typ)
    # of PolyTypeKind.Intersection:
    #     Value.Intersection(typ.types.map(inst))
    typ

proc compose(a, b: Substitution): Substitution =
    result = initTable[TypeVar, ref Value]()
    for key, value in b:
        result[key] = a.apply(value)
    for key, value in a:
        if key notin result:
            result[key] = value
        else:
            if value.kind == ValueKind.Var:
                if value.tv notin result:
                    result[value.tv] = Value.Var(key)
                else:
                    raise newException(TypeError, "")
proc `@`*(a, b: Substitution): Substitution =
    compose(a, b)
