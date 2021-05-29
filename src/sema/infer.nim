
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
    TypeVar(id: newTypeVarId(), overloads: @[])

proc newVar(typ: typedesc[Type]): Type =
    let v = newTypeVar()
    Type(kind: TypeKind.Var, v: v)


type
    Scope* = Table[string, seq[Symbol]]
    TypeEnv* = ref object
        scope*: Scope
        scopes*: seq[Scope]
        subs*: Substitution
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
    initTable[string, seq[Symbol]]()

proc `$`*(self: Scope): string =
    $self

let
    nullSubst = initTable[TypeVar, Type]()
    nullFtv = initHashSet[TypeVar]()
    emptyUnifier = (nullSubst, newSeq[Constraint]())

# TypeEnv
proc newTypeEnv*(): TypeEnv =
    TypeEnv(
        scope: newScope(),
        subs: nullSubst,
        cons: @[]
    )

proc apply(self: Substitution, t: PolyType): PolyType
proc apply(self: Substitution, t: Type): Type =
    case t.kind
    of TypeKind.Unit..TypeKind.String:
        t
    of TypeKind.Arr:
        Type.Arr(t.paramty.mapIt(self.apply(it)), self.apply(t.rety))
    of TypeKind.Var:
        if t.v in self:
            # TODO: check overloads
            self[t.v]
        else:
            var res = t
            res.v.overloads = t.v.overloads.mapIt(self.apply(it))
            res
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
        t.typ = self.apply(t.typ)
    of SymbolKind.Choice:
        t.typ = self.apply(t.typ)
        t.syms = t.syms.mapIt(self.apply(it))
    t
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

proc apply(self: Substitution, t: Table[string, seq[Symbol]]): Table[string, seq[Symbol]] =
    result = t
    for key, value in result.pairs:
        result[key] = self.apply(value)
proc ftv(self: Table[string, seq[Symbol]]): HashSet[TypeVar] =
    toSeq(self.values).ftv

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
    result = case typ.kind
    of PolyTypeKind.ForAll:
        let
            gen = toSeq(typ.gen.items)
            s = gen.zip(gen.mapIt(Type.newVar())).toTable()
        s.apply(typ.typ)

proc occursIn(tv: TypeVar, typ: Type): bool =
    tv in ftv(typ)
proc unify(env: TypeEnv, t1, t2: Type): Substitution
proc unifyMany(env: TypeEnv, cs: seq[(Type, Type)]): Substitution
    
proc bindt(env: TypeEnv, tv: TypeVar, typ2: Type): Substitution =
    if tv.occursIn(typ2):
        raise newException(TypeError, "Infinite Type")
    else:
        case typ2.kind
        of TypeKind.TypeDesc:
            let overloads = tv.overloads
            env.unify(overloads.filterIt(it.typ.kind == TypeKind.TypeDesc)[0].inst, typ2)
        else:
            @[(tv, typ2)].toTable()
        # case tv.kind.kind
        # of KindKind.Top:
        #     @[(tv, typ2)].toTable()
        # of KindKind.OverLoad:
        #     @[(tv, typ2)].toTable()
        # of KindKind.Singleton:
        #     let typ = tv.kind.typ
        #     assert typ.kind == typ2.kind, fmt"{typ} and {typ2} can not be unified"
        #     env.unify(typ, typ2)

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
            if t1.v == t2.v:
                nullSubst
            elif t1.v.id < t2.v.id:
                env.bindt(t2.v, t1)
            else:
                env.bindt(t1.v, t2)
        of TypeKind.TypeDesc:
            env.unify(t1.typ, t2.typ)
    elif t1.kind == TypeKind.Var:
        env.bindt t1.v, t2
    elif t2.kind == TypeKind.Var:
        env.bindt t2.v, t1
    else:
        assert false, fmt"{t1} and {t2} can not be unified"
        nullSubst

proc uni(env: TypeEnv, t1, t2: Type) =
    env.cons.add((t1, t2))

proc pushScope(self: TypeEnv) =
    self.scopes.add self.scope
    var scope = self.scope
    self.scope = scope
proc popScope(self: TypeEnv) =
    self.scope = self.scopes.pop
proc extend(self: TypeEnv, name: Identifier, sym: Symbol) =
    name.symbol = self.subs.apply sym
    if name.name notin self.scope:
        self.scope[name.name] = @[]
    var syms = self.scope[name.name]
    case sym.kind
    of SymbolKind.Var..SymbolKind.Const:
        syms = syms.filterIt(it.kind notin {SymbolKind.Var..SymbolKind.Const})
        syms.add sym
    of SymbolKind.Func:
        syms.add sym
    of SymbolKind.Type:
        syms = syms.filterIt(it.kind != SymbolKind.Type)
        syms.add sym
    of SymbolKind.Choice:
        assert false
    self.scope[name.name] = syms
proc lookup(self: TypeEnv, name: string): Symbol =
    let syms = self.scope[name]
    case syms.len
    of 0:
        assert false, fmt"{name} was not declared"
        nil
    of 1:
        syms[0]
    else:
        # TODO: f
        let
            kinds = syms.mapIt(it.typ)
            # kinds = syms.mapIt(Kind(kind: KindKind.Singleton, typ: it.typ.inst))
            # kind = Kind(kind: KindKind.OverLoad, kinds: kinds.toHashSet)
            tv = newTypeVar()
        tv.overloads = kinds
        # tv.kind = kind
        Symbol.Choice(PolyType.ForAll(nullFtv, Type.Var(tv)), syms)

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

proc infer*(self: Term, env: var TypeEnv): Type =
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
        env.subs.apply self.id.symbol.typ.inst
    of TermKind.Let:
        let
            t1 = self.default.infer(env)
            sym = if t1.kind == TypeKind.Arr:
                Symbol.Let(t1.gen(env))
            else:
                Symbol.Let(PolyType.ForAll(nullFtv, t1))
            # sym = Symbol.Let(t1.gen(env))
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
        env.subs = env.unifyMany(paramty.zip(tvs.mapIt(Type.TypeDesc(it)))) @ env.subs
        # for (pt, tv) in paramty.zip(tvs):
        #     env.uni(Type.TypeDesc(tv), pt)
        env.subs = env.unify(Type.TypeDesc(tv), rety) @ env.subs
        env.pushScope
        for (name, tv) in paramname.zip(tvs):
            let sym = Symbol.Let(PolyType.ForAll(nullFtv, tv))
            env.extend(name, sym)
        let rety2 = fn.body.infer(env)
        env.subs = (if not metadata.isNil and metadata.kind == MetadataKind.ImportLL:
            env.unify(Type.Unit, rety2)
        else:
            env.unify(tv, rety2)) @ env.subs
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
        env.subs = env.unify(callee, Type.Arr(args, tv)) @ env.subs
        tv
    of TermKind.If:
        let
            cond = self.cond.infer(env)
            thent = self.thent.infer(env)
            elset = self.elset.infer(env)
        env.subs = env.unify(cond, Type.Bool) @ env.subs
        env.subs = env.unify(thent, elset) @ env.subs
        thent
    of TermKind.Seq:
        let
            ts = self.ts.mapIt(it.infer(env))
        for e in ts[0..^2]:
            env.subs = env.unify(e, Type.Unit) @ env.subs
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
        # s = env.solve
        s = env.subs
    env = s.apply(env)
    result = s.apply(result)
    s.apply(self)

when isMainModule:
    assert Type is Substituable
    assert seq[Type] is Substituable
    var
        env = newTypeEnv()
    let
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