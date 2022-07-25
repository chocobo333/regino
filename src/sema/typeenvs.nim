
import sets
import tables
import options
import sequtils
import algorithm

import ir
import errors
import generators
import ir/inst
import ir/tos

import ../orders
import ../utils

type
    Constraint* = (Type, Type)
    TypeEnv* = ref object
        id_gen: Generator[VarId]
        tvs*: HashSet[Type]
        selects*: HashSet[Type]
        constraints*: seq[Constraint]
        scope*: Scope
        order*: Order[Type]

proc newScope*(parent: Scope = nil): Scope =
    Scope(
        parent: parent,
        vars: initTable[string, Symbol](),
        types: initTable[string, Symbol](),
        funcs: initTable[string, seq[Symbol]](),
        converters: initTable[(Type, Type), Ident](),
    )

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

proc getConverters*(self: Scope): Table[(Type, Type), Ident] =
    for scope in self:
        for (k, v) in scope.converters.pairs:
            if not result.contains(k):
                result[k] = v

proc newTypeEnv*(scope: Scope): TypeEnv =
    TypeEnv(
        id_gen: newGenerator(0),
        tvs: initHashSet[Type](),
        selects: initHashSet[Type](),
        scope: scope,
        order: newOrder[Type]()
    )

proc pushScope*(self: TypeEnv, scope: Scope) =
    assert scope.parent == self.scope
    self.scope = scope
proc popScope*(self: TypeEnv): Scope {.discardable.} =
    result = self.scope
    self.scope = self.scope.parent

template enter*(self: TypeEnv, scope: Scope, body: untyped): untyped =
    block:
        self.pushScope(scope)
        defer:
            self.popScope
        body

proc addSymbol*(self: TypeEnv, symbol: Symbol): Option[Error] =
    let
        name = symbol.ident.name
    case symbol.kind
    of SymbolKind.Notdeclared:
        result = some Error.InternalError
    of SymbolKind.Let, SymbolKind.Var, SymbolKind.Const, SymbolKind.Param:
        if name in self.scope.types:
            result = some Error.Redifinition
        self.scope.vars[name] = symbol
    of SymbolKind.Type, SymbolKind.GenParam:
        if name in self.scope.types or name in self.scope.funcs or name in self.scope.vars:
            result = some Error.Redifinition
        self.scope.types[name] = symbol
    of SymbolKind.Func, SymbolKind.Field:
        if name in self.scope.types:
            result = some Error.Redifinition
        if name in self.scope.funcs:
            self.scope.funcs[name].add symbol
        else:
            self.scope.funcs[name] = @[symbol]

proc lookupVar*(self: TypeEnv, name: string): seq[Symbol] =
    for scope in self.scope:
        if name in scope.vars:
            return @[scope.vars[name]]
proc lookupType*(self: TypeEnv, name: string): seq[Symbol] =
    for scope in self.scope:
        if name in scope.types:
            return @[scope.types[name]]
proc lookupFunc*(self: TypeEnv, name: string): seq[Symbol] =
    for scope in self.scope:
        if name in scope.funcs:
            result.add scope.funcs[name]
proc newVar(self: TypeEnv): Type =
    let id = self.id_gen.get()
    result = Type(
        kind: TypeKind.Var,
        id: id,
        ub: Type.Unit,
        lb: Type.Bottom
    )
    self.tvs.incl result
proc newSelect(self: TypeEnv, choices: HashSet[Type]): Type =
    let id = self.id_gen.get()
    result = Type(
        kind: TypeKind.Select,
        id: id,
        choices: choices
    )
    self.selects.incl result
proc newGen(self: TypeEnv, ub: Type = Type.Unit, typ: Type = Type.Univ(0)): GenericType =
    let id = self.id_gen.get()
    GenericType(
        id: id,
        ub: ub,
        typ: typ
    )
proc Var*(_: typedesc[Type], env: TypeEnv): Type =
    env.newVar()
proc Select*(_: typedesc[Type], choices: HashSet[Type], env: TypeEnv): Type =
    env.newSelect(choices)
proc Select*(_: typedesc[Type], choices: seq[Type], env: TypeEnv): Type =
    env.newSelect(choices.toHashSet)
proc Gen*(_: typedesc[Type], env: TypeEnv, ub: Type = Type.Unit, typ: Type = Type.Univ(0)): Type =
    Type.Gen(env.newGen(ub, typ))

proc inst*(self: PiType, env: TypeEnv): Type =
    var subs = initTable[GenericType, Type]()
    for k in self.params:
        subs[k] = Type.Var(env)
    self.rety.inst(subs)

template init_cons*(self: TypeEnv, body: untyped): untyped =
    let
        tmpCons = self.constraints
        tmpOrder = self.order
        tmpTvs = self.tvs
        tmpSelects = self.selects
    block:
        defer:
            self.constraints = tmpCons
            self.order = tmpOrder
            self.tvs = tmpTvs
            self.selects = tmpSelects
        self.constraints = @[]
        self.order.primal.clear()
        self.order.dual.clear()
        self.tvs = initHashSet[Type]()
        self.selects = initHashSet[Type]()
        body
