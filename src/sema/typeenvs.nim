
import sets
import tables
import options

import ir
import errors
import generators
import ir/inst

import ../orders

type
    Constraint = (Type, Type)
    TypeEnv* = ref object
        id_gen: Generator[VarId]
        tvs*: HashSet[TypeVar]
        selects*: HashSet[TypeVar]
        constraints*: seq[Constraint]
        scope*: Scope
        order*: Order[Type]

proc newScope*(parent: Scope = nil): Scope =
    Scope(
        parent: parent,
        vars: initTable[string, Symbol](),
        types: initTable[string, Symbol](),
        funcs: initTable[string, seq[Symbol]]()
    )

proc newTypeEnv*(scope: Scope): TypeEnv =
    TypeEnv(
        id_gen: newGenerator(0),
        tvs: initHashSet[TypeVar](),
        selects: initHashSet[TypeVar](),
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

iterator items*(self: Scope): Scope =
    var self = self
    while not self.isNil:
        yield self
        self = self.parent
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
proc newVar(self: TypeEnv): TypeVar =
    let id = self.id_gen.get()
    result = TypeVar(
        kind: TypeVarKind.Var,
        id: id,
        ub: Type.Unit,
        lb: Type.Bottom
    )
    self.tvs.incl result
proc newSelect(self: TypeEnv, choices: HashSet[Type]): TypeVar =
    let id = self.id_gen.get()
    result = TypeVar(
        kind: TypeVarKind.Select,
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
    Type.Var(env.newVar())
proc Select*(_: typedesc[Type], choices: HashSet[Type], env: TypeEnv): Type =
    Type.Var(env.newSelect(choices))
proc Select*(_: typedesc[Type], choices: seq[Type], env: TypeEnv): Type =
    Type.Var(env.newSelect(choices.toHashSet))
proc Gen*(_: typedesc[Type], env: TypeEnv, ub: Type = Type.Unit, typ: Type = Type.Univ(0)): Type =
    Type.Gen(env.newGen(ub, typ))

proc inst*(self: PiType, env: TypeEnv): Type =
    var subs = initTable[GenericType, Type]()
    for k in self.params:
        subs[k] = Type.Var(env)
    self.rety.inst(subs)
