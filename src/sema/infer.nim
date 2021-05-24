
import tables
import sets
import hashes
import strformat
import sequtils

import il
import types
import symbols


var
    cur_tvid = 0

proc newTypeVarId: int =
    inc cur_tvid
    cur_tvid
proc newTypeVar: TypeVar =
    TypeVar(id: newTypeVarId())

proc newVar(typ: typedesc[Type]): Type =
    let v = TypeVar(id: newTypeVarId())
    Type(kind: TypeKind.Var, v: v)


type
    Scope* = ref object
        parent*: Scope
        vars*: Table[string, Symbol]
    TypeEnv* = ref object
        scope*: Scope
        cons*: seq[Constraint]
    Substitution* = Table[TypeVar, Type]
    Substituable* = concept T, var t
        apply(Substitution, T)
        t.ftv() is HashSet[TypeVar]
    Constraint* = (Type, Type)
    Unifier* = (Substitution, seq[Constraint])
    TypeError* = object of CatchableError

# Scope
proc newScope*(): Scope =
    Scope(vars: initTable[string, Symbol]())

proc `$`*(self: Scope): string =
    $self.vars

# TypeEnv
proc newTypeEnv*(): TypeEnv =
    TypeEnv(
        scope: newScope(),
        cons: @[]
    )

let
    nullSubst = initTable[TypeVar, Type]()
    nullFtv = initHashSet[TypeVar]()
    emptyUnifier = (nullSubst, newSeq[Constraint]())

proc apply(self: Substitution, t: Type): Type =
    case t.kind
    of TypeKind.Unit..TypeKind.String:
        t
    of TypeKind.Arr:
        Type.Arr(t.paramty.mapIt(self.apply(it)), self.apply(t.rety))
    of TypeKind.Var:
        if t.v in self:
            self[t.v]
        else:
            t
    of TypeKind.TypeDesc:
        Type.TypeDesc(self.apply(t.typ))
proc ftv(self: Type): HashSet[TypeVar] =
    case self.kind
    of TypeKind.Unit..TypeKind.String:
        initHashSet[TypeVar]()
    of TypeKind.Arr:
        self.paramty.map(ftv).foldl(a+b, initHashSet[TypeVar]()) + self.rety.ftv
    of TypeKind.Var:
        @[self.v].toHashSet()
    of TypeKind.TypeDesc:
        self.typ.ftv
proc apply(self: Substitution, t: PolyType): PolyType =
    case t.kind
    of PolyTypeKind.ForAll:
        var s = self
        for e in t.gen:
            s.del(e)
        PolyType.ForAll(t.gen, s.apply(t.typ))
proc ftv(self: PolyType): HashSet[TypeVar] =
    case self.kind
    of PolyTypeKind.ForAll:
        self.typ.ftv - self.gen
proc apply(self: Substitution, t: Symbol): Symbol =
    case t.kind
    of SymbolKind.Var..SymbolKind.Type:
        let kind: range[SymbolKind.Var..SymbolKind.Type] = t.kind
        Symbol(kind: kind, typ: self.apply(t.typ), val: t.val)
    of SymbolKind.Choice:
        Symbol.Choice(self.apply(t.typ), t.syms.mapIt(self.apply(it)))
proc ftv(self: Symbol): HashSet[TypeVar] =
    case self.kind
    of SymbolKind.Var..SymbolKind.Type:
        self.typ.ftv
    of SymbolKind.Choice:
        self.typ.ftv + self.syms.map(ftv).foldl(a+b, initHashSet[TypeVar]())

proc apply[T: Substituable](self: Substitution, t: seq[T]): seq[T] =
    t.mapIt(self.apply(it))
proc ftv[T: Substituable](self: seq[T]): HashSet[TypeVar] =
    let tmp = self.map(ftv).foldl(a+b, initHashSet[TypeVar]())

proc apply(self: Substitution, t: Table[string, Symbol]): Table[string, Symbol] =
    result = t
    for key, value in result.pairs:
        result[key] = self.apply(value)
proc ftv(self: Table[string, Symbol]): HashSet[TypeVar] =
    toSeq(self.values).ftv

proc apply(self: Substitution, t: Scope): Scope =
    if t.isNil:
        return nil
    result = newScope()
    result.vars = self.apply(t.vars)
    result.parent = self.apply(t.parent)
proc ftv(self: Scope): HashSet[TypeVar] =
    if self.isNil:
        return nullFtv
    self.vars.ftv + self.parent.ftv
proc apply(self: Substitution, t: TypeEnv): TypeEnv =
    result = newTypeEnv()
    result.cons = t.cons
    result.scope = self.apply(t.scope)
proc ftv(self: TypeEnv): HashSet[TypeVar] =
    self.scope.ftv

proc apply(self: Substitution, t: Constraint): Constraint =
    (self.apply(t[0]), self.apply(t[1]))

proc apply(self: Substitution, t: seq[Constraint]): seq[Constraint] =
    t.mapIt(self.apply(it))

proc compose(a, b: Substitution): Substitution =
    result = initTable[TypeVar, Type]()
    for key, value in b:
        result[key] = a.apply(value)
    for key, value in a:
        if key notin result:
            result[key] = value

proc `@`(a, b: Substitution): Substitution =
    compose(a, b)

proc gen(typ: Type, env: TypeEnv): PolyType =
    let
        gen = typ.ftv - env.ftv
    PolyType.ForAll(gen, typ)
proc inst(typ: PolyType): Type =
    case typ.kind
    of PolyTypeKind.ForAll:
        let
            gen = toSeq(typ.gen.items)
            s = gen.zip(gen.mapIt(Type.newVar())).toTable()
        s.apply(typ.typ)

proc occursIn(tv: TypeVar, typ: Type): bool =
    tv in ftv(typ)
proc bindt(tv: TypeVar, typ: Type): Substitution =
    if tv.occursIn(typ):
        raise newException(TypeError, "Infinite Type")
    else:
        @[(tv, typ)].toTable()

proc unify(env: TypeEnv, t1, t2: Type): Substitution
proc unifyMany(env: TypeEnv, cs: seq[(Type, Type)]): Substitution =
    if cs.len == 0:
        nullSubst
    else:
        let
            (c, cs) = (cs[0], cs[1..^1])
            (t1, t2) = c
            s1 = env.unify(t1, t2)
            s2 = env.unifyMany(s1.apply(cs))
        s2 @ s1

proc unify(env: TypeEnv, t1, t2: Type): Substitution =
    if t1 == t2:
        return nullSubst
    if t1.kind == t2.kind:
        case t1.kind
        of TypeKind.Unit..TypeKind.String:
            nullSubst
        of TypeKind.Arr:
            assert t1.paramty.len == t2.paramty.len, fmt"{t1} and {t2} can not be unified"
            env.unifyMany(t1.paramty.zip(t2.paramty) & (t1.rety, t2.rety))
        of TypeKind.Var:
            if t1.v.id == t2.v.id:
                nullSubst
            elif t1.v.id < t2.v.id:
                bindt(t2.v, t1)
            else:
                bindt(t1.v, t2)
        of TypeKind.TypeDesc:
            env.unify(t1.typ, t2.typ)
    elif t1.kind == TypeKind.Var:
        bindt t1.v, t2
    elif t2.kind == TypeKind.Var:
        bindt t2.v, t1
    else:
        assert false, fmt"{t1} and {t2} can not be unified"
        nullSubst

proc uni(env: TypeEnv, t1, t2: Type) =
    env.cons.add((t1, t2))

proc pushScope(self: TypeEnv) =
    let tmp = self.scope
    self.scope = newScope()
    self.scope.parent = tmp
proc popScope(self: TypeEnv) =
    let tmp = self.scope.parent
    self.scope = tmp
proc extend(self: TypeEnv, name: Identifier, sym: Symbol) =
    name.symbol = sym
    self.scope.vars[name.name] = sym
proc lookup(self: TypeEnv, name: string): Symbol =
    var scope = self.scope
    while not scope.isNil:
        if name in scope.vars:
            return scope.vars[name]
        scope = scope.parent
    assert false, fmt"{name} was not declared"

# proc asType(self: Term): Type =
#     result = if self.kind == TermKind.Id:
#         case self.name
#         of "int":
#             Type.Int
#         else:
#             assert false, ""
#             nil
#     else:
#         assert false, ""
#         nil
#     self.typ = result

proc infer*(self: Term, env: TypeEnv): Type =
    result = case self.kind
    of TermKind.Unit:
        Type.Unit
    of TermKind.Bool:
        Type.Bool
    of TermKind.Int:
        Type.Int
    of TermKind.String:
        Type.String
    of TermKind.Id:
        self.id.symbol = env.lookup(self.id.name)
        self.id.symbol.typ.inst
    of TermKind.Let:
        let
            t1 = self.default.infer(env)
            sym = Symbol.Let(PolyType.ForAll(nullFtv, t1))
        env.extend(self.name, sym)
        Type.Unit
    of TermKind.TypeDef:
        let
            t1 = self.default.infer(env)
            sym = Symbol.Typ(PolyType.ForAll(nullFtv, t1))
        env.extend(self.name, sym)
        Type.Unit
    of TermKind.FuncDef:
        let
            fn = self.fn
            paramname = fn.params.mapit(it.name)
            paramty = fn.params.mapIt(it.typ.infer(env))
            rety = fn.rety.infer(env)
            metadata = fn.metadata
            tvs = paramty.mapIt(Type.newVar())
            tv = Type.newVar()
            sym = Symbol.Func(PolyType.ForAll(nullFtv, Type.Arr(tvs, tv)))
        env.extend(fn.id, sym)
        for (pt, tv) in paramty.zip(tvs):
            env.uni(Type.TypeDesc(tv), pt)
        env.uni(Type.TypeDesc(tv), rety)
        env.pushScope
        for (name, tv) in paramname.zip(tvs):
            let sym = Symbol.Let(PolyType.ForAll(nullFtv, tv))
            env.extend(name, sym)
        let rety2 = fn.body.infer(env)
        if  not metadata.isNil and metadata.kind == MetadataKind.ImportLL:
            env.uni(Type.Unit, rety2)
        else:
            env.uni(tv, rety2)
        env.popScope
        Type.Unit
    of TermKind.Lam:
        let
            param = self.param
            body = self.body
            tv = Type.newVar()
            sym = Symbol.Let(PolyType.ForAll(nullFtv, tv))
        env.pushScope
        env.extend(self.param, sym)
        let t = body.infer(env)
        env.popScope
        Type.Arr(@[tv], t)
    of TermKind.App:
        let
            callee = self.callee.infer(env)
            args = self.args.mapIt(it.infer(env))
            tv = Type.newVar()
        env.uni(callee, Type.Arr(args, tv))
        tv
    of TermKind.If:
        let
            cond = self.cond.infer(env)
            thent = self.thent.infer(env)
            elset = self.elset.infer(env)
        env.uni(cond, Type.Bool)
        env.uni(thent, elset)
        thent
    of TermKind.Seq:
        let
            ts = self.ts.mapIt(it.infer(env))
        for e in ts[0..^2]:
            env.uni(e, Type.Unit)
        ts[^1]
    of TermKind.TypeOf:
        let t = self.typeof.infer(env)
        Type.TypeDesc(t)
    of TermKind.Metadata:
        if not self.metadata.param.isNil:
            discard self.metadata.param.infer(env)
        Type.Unit
    self.typ = result

proc solve*(env: TypeEnv, s: Substitution = nullSubst, cs: seq[Constraint] = env.cons): Substitution =
    if cs == @[]:
        s
    else:
        let
            (c, cs0) = (cs[0], cs[1..^1])
            (t1, t2) = c
            s1 = env.unify(t1, t2)
        env.solve(s1 @ s, s1.apply(cs0))

proc apply(s: Substitution, t: Term) =
    t.typ = s.apply(t.typ)
    case t.kind
    of TermKind.Unit..TermKind.Id:
        discard
    of TermKind.Let..TermKind.TypeDef:
        s.apply(t.default)
    of TermKind.FuncDef:
        for e in t.fn.params:
            s.apply(e.typ)
        s.apply(t.fn.rety)
        s.apply(t.fn.body)
    of TermKind.Lam:
        s.apply(t.body)
    of TermKind.App:
        s.apply(t.callee)
        for e in t.args:
            s.apply(e)
    of TermKind.If:
        s.apply(t.cond)
        s.apply(t.thent)
        s.apply(t.elset)
    of TermKind.Seq:
        for e in t.ts:
            s.apply(e)
    of TermKind.TypeOf:
        s.apply(t.typeof)
    of TermKind.Metadata:
        s.apply(t.metadata.param)
proc typeInfer*(self: Term, env: var TypeEnv): Type =
    result = self.infer(env)
    let
        s = env.solve
    env = s.apply(env)
    result = s.apply(result)
    s.apply(self)

when isMainModule:
    assert Type is Substituable
    assert seq[Type] is Substituable
    let
        env = newTypeEnv()
        i0 = Term.Int(0)
        i1 = Term.Int(1)
        i3 = Term.Int(3)
        x = "x"
        f = "f"
        id_x = Term.Id(x)
        id_f = Term.Id(f)
        ift = Term.If(id_x, i1, i0)
        a = Term.Lam(x, ift)
        b = Term.Seq(@[Term.Let(f, a), Term.App(id_f, @[Term.Bool(true)])])
    echo b
    let
        rety = b.infer(env)
        s = env.solve
    s.apply(b)
    echo id_f.typ

export `$`