
import tables
import sets
import hashes
import strformat
import sequtils

import il
import types


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
        vars*: Table[string, PolyType]
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
    Scope(vars: initTable[string, PolyType]())

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
        Type.Arr(self.apply(t.paramty), self.apply(t.rety))
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
        self.paramty.ftv + self.rety.ftv
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

proc apply[T: Substituable](self: Substitution, t: seq[T]): seq[T] =
    t.mapIt(self.apply(it))
proc ftv[T: Substituable](self: seq[T]): HashSet[TypeVar] =
    let tmp = self.map(ftv).foldl(a+b, initHashSet[TypeVar]())

proc apply(self: Substitution, t: Table[system.string, types.PolyType]): Table[system.string, types.PolyType] =
    result = t
    for key, value in result.pairs:
        result[key] = self.apply(value)
proc ftv(self: Table[system.string, types.PolyType]): HashSet[TypeVar] =
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
            env.unifyMany(@[(t1.paramty, t2.paramty), (t1.rety, t2.rety)])
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
proc extend(self: TypeEnv, name: string, typ: PolyType) =
    self.scope.vars[name] = typ
proc lookup(self: TypeEnv, name: string): PolyType =
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
        env.lookup(self.name).inst
    of TermKind.Let:
        let
            t1 = self.default.infer(env)
        env.extend(self.id, PolyType.ForAll(nullFtv, t1))
        Type.Unit
    of TermKind.FuncDef:
        let
            paramty = self.paramty.infer(env)
            rety = self.rety.infer(env)
            tv1 = Type.newVar()
            tv2 = Type.newVar()
        env.extend(self.fname, PolyType.ForAll(nullFtv, Type.Arr(tv1, tv2)))
        env.uni(Type.TypeDesc(tv1), paramty)
        env.uni(Type.TypeDesc(tv2), rety)
        env.pushScope
        env.extend(self.paramname, PolyType.ForAll(nullFtv, tv1))
        let rety2 = self.fbody.infer(env)
        env.uni(tv2, rety2)
        env.popScope
        Type.Unit
    of TermKind.Lam:
        let
            param = self.param
            body = self.body
            tv = Type.newVar()
        env.pushScope
        env.extend(self.param, PolyType.ForAll(nullFtv, tv))
        let t = body.infer(env)
        env.popScope
        Type.Arr(tv, t)
    of TermKind.App:
        let
            callee = self.callee.infer(env)
            arg = self.arg.infer(env)
            tv = Type.newVar()
        env.uni(callee, Type.Arr(arg, tv))
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
    of TermKind.Let:
        s.apply(t.default)
    of TermKind.FuncDef:
        s.apply(t.paramty)
        s.apply(t.rety)
        s.apply(t.fbody)
    of TermKind.Lam:
        s.apply(t.body)
    of TermKind.App:
        s.apply(t.callee)
        s.apply(t.arg)
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
        b = Term.Seq(@[Term.Let(f, a), Term.App(id_f, Term.Bool(true))])
    echo b
    let
        rety = b.infer(env)
        s = env.solve
    s.apply(b)
    echo id_f.typ

export `$`