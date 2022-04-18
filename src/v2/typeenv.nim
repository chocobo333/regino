
import sets
import tables
import sequtils
import options
import strformat
import sugar

import il
import errors
import orders
import utils


type
    TypeEnv* = ref object
        scope*: Scope
        tvs*: HashSet[Value]
        constraints*: seq[Constraint]
        tvconstraints*: seq[Constraint]
        interconstraints*: seq[Constraint]
        errs*: seq[TypeError]
    Constraint* = (Value, Value)   # for t1 <= t2

# converter to*(self: (Value, Value)): Constraint =
#     (self[0], self[1], TypeError.Subtype(self[0], self[1]))
proc `==`*(self, other: Constraint): bool =
    self[0] == other[0] and self[1] == other[1]
# proc `$`*(self: Constraint): string =
#     $(self[0], self[1])

template `raise`*(self: TypeEnv, msg: string): untyped =
    self.errs.add TypeError.Internal(msg)


proc Var*(_: typedesc[Value], env: TypeEnv): Value =
    result = Value(kind: ValueKind.Var, tv: newTypeVar())
    env.tvs.incl result

proc newTypeEnv*(scope: Scope): TypeEnv =
    TypeEnv(
        scope: scope,
    )

proc pushScope*(env: TypeEnv, scope: Scope) =
    assert scope.parent == env.scope
    env.scope = scope

proc popScope*(env: TypeEnv): Scope {.discardable.} =
    result = env.scope
    env.scope = env.scope.parent

template enter*(env: TypeEnv, scope: Scope, body: untyped): untyped =
    block:
        env.pushScope scope
        defer:
            env.popScope
        body

proc lookupId*(self: TypeEnv, name: string, kinds: set[SymbolKind] = {SymbolKind.low..SymbolKind.high}): seq[Symbol] =
    var
        tmp: set[SymbolKind]
        varsKind = {SymbolKind.Let, SymbolKind.Var, SymbolKind.Const, SymbolKind.Param}
        typesKind = {SymbolKind.Typ, SymbolKind.GenParam}
        funcsKind = {SymbolKind.Func}
    for scope in self.scope:
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

proc addIdent*(self: TypeEnv, sym: Symbol) =
    let
        name = sym.id.name
    if name in self.scope.syms:
        self.scope.syms[name].add sym
    else:
        self.scope.syms[name] = @[sym]


proc inst*(typ: Value, env: TypeEnv, subs: Table[GenericType, Value] = initTable[GenericType, Value]()): Value =
    let sym = typ.symbol
    result = case typ.kind
    of ValueKind.Literal..ValueKind.String:
        typ.deepCopy
    of ValueKind.Pair:
        Value.Pair(typ.first.inst(env, subs), typ.second.inst(env, subs))
    of ValueKind.Array:
        Value.Array(typ.base.inst(env, subs))
    of ValueKind.Singleton:
        Value.Singleton(typ.base.inst(env, subs))
    of ValueKind.Distinct:
        Value.Distinct(typ.base.inst(env, subs))
    of ValueKind.ArrayV:
        Value.Array(typ.vals.mapIt(it.inst(env, subs)))
    of ValueKind.Record:
        Value.Record(typ.members.map(it => it.inst(env, subs)))
    of ValueKind.Ptr:
        Value.Ptr(typ.pointee.inst(env, subs))
    of ValueKind.Pi:
        let newSubs = subs.merge(
                typ.implicit.filterIt(it notin subs).mapIt(block:
                    let v = Value.Var(env)
                    (it, v)
                ).toTable
            )
        Value.Arrow(
            typ.params.map(it => it.inst(env, newsubs)),
            typ.rety.inst(env, newsubs)
        )
    of ValueKind.Sum:
        Value.Sum(typ.cons.map(it => it.inst(env, subs)))
    of ValueKind.Trait:
        Value.trait(
            (typ.paty[0], typ.paty[1].inst(env, subs)),
            typ.iss.mapIt((it[0], it[1].inst(env, subs))),
            typ.fns
        )
    of ValueKind.Intersection:
        Value.Intersection(typ.types.map(it => it.inst(env, subs)))
    of ValueKind.Union:
        Value.Union(typ.types.map(it => it.inst(env, subs)))
    of ValueKind.Cons:
        let newSubs = subs.merge(
                typ.implicit.filterIt(it notin subs).mapIt(block:
                    let v = Value.Var(env)
                    (it, v)
                ).toTable
            )
        typ.rety.inst(env, newSubs)
    of ValueKind.Var:
        typ
    of ValueKind.Gen:
        if typ.gt in subs:
            subs[typ.gt]
        else:
            typ
    of ValueKind.Link:
        Value.Link(typ.inst(env, subs))
    result.symbol = sym


    # let sym = typ.symbol
    # result = case typ.kind
    # of ValueKind.Bottom..ValueKind.String:
    #     typ.deepCopy
    # of ValueKind.BoolV..ValueKind.StringV:
    #     typ.deepCopy
    # # of ValueKind.List:
    # #     nil
    # of ValueKind.Ptr:
    #     Value.Ptr(typ.pointee.inst(env, subs))
    # of ValueKind.Pair:
    #     Value.Pair(typ.first.inst(env, subs), typ.second.inst(env, subs))
    # # of ValueKind.Tuple:
    # #     nil
    # of ValueKind.Record:
    #     Value.Record(toSeq(typ.members.pairs).mapIt((it[0], it[1].inst(env, subs))))
    # of ValueKind.Pi:
    #     let
    #         subs = typ.genty.map(it => (block:
    #             let t = Value.Var(env)
    #             t.tv.ub = it[1].gen.ub
    #             (it[1].gen, t)
    #         ))
    #         ret = Value.Arrow(typ.paramty.mapIt(it.inst(env, subs.toTable)), typ.rety.inst(env, subs.toTable))
    #     ret.gentyinst = subs.mapIt(it[1])
    #     ret
    # # of ValueKind.Sigma:
    # #     nil
    # of ValueKind.Typedesc:
    #     Value.Typedesc(typ.`typedesc`.inst(env, subs))
    # # of ValueKind.Distinct:
    # #     nil
    # of ValueKind.Var:
    #     typ
    # of ValueKind.Intersection:
    #     Value.Intersection(typ.types.map(it => it.inst(env, subs)))
    # of ValueKind.Union:
    #     Value.Union(typ.types.map(it => it.inst(env, subs)))
    # of ValueKind.Link:
    #     Value.Link(typ.to.inst(env, subs))
    # of ValueKind.Gen:
    #     if typ.gen in subs:
    #         subs[typ.gen]
    #     else:
    #         typ
    # result.symbol = sym

    # typ

proc `<=`*(t1, t2: Value): bool {.error.}
proc `<=`*(env: TypeEnv, t1, t2: Value): bool =
    if t1.kind == ValueKind.Link:
        env.`<=`(t1.to, t2)
    elif t2.kind == ValueKind.Link:
        env.`<=`(t1, t2.to)
    elif t1.kind == ValueKind.Bottom:
        true
    elif t2.kind == ValueKind.Unit:
        true
    elif t1 == t2:
        true
    elif t1.kind == ValueKind.Gen:
        env.`<=`(t1.gt.ub, t2)
    elif t2.isUniv:
        case t1.kind
        of ValueKind.Unit:
            true
        of ValueKind.Pair:
            `<=`(env, t1.first, t2) and `<=`(env, t1.second, t2)
        of ValueKind.Ptr:
            env.`<=`(t1.pointee, t2)
        of ValueKind.Singleton:
            env.`<=`(t1.base, t2)
        else:
            if t1.isUniv:
                t1.litval.level <= t2.litval.level
            else:
                debug t1.kind
                env.raise("notimplemented")
                false
    elif t1.kind == t2.kind:
        case t1.kind
        of ValueKind.Literal:
            false
        of ValueKind.Bottom:
            true
        of ValueKind.Unit:
            true
        of ValueKind.Bool:
            true
        of ValueKind.Integer:
            t1.bits <= t2.bits
        of ValueKind.Float:
            true
        of ValueKind.Char:
            true
        of ValueKind.String:
            true
        of ValueKind.Pair:
            `<=`(env, t1.first, t2.first) and `<=`(env, t1.second, t2.second)
        of ValueKind.Record:
            var res = true
            for key in t2.members.keys:
                if key in t1.members:
                    if env.`<=`(t1.members[key], t2.members[key]):
                        discard
                    else:
                        res = false
                        break
                else:
                    res = false
                    break
            res
        of ValueKind.Pi:
            # TODO: genty
            let
                params = t1.params.zip(t2.params).allIt(env.`<=`(it[1], it[0]))
                rety = env.`<=`(t1.rety, t2.rety)
            params and rety
        of ValueKind.Singleton:
            env.`<=`(t1.base, t2.base)
        of ValueKind.Var:
            env.`<=`(t1.tv.ub, t2.tv.lb)
        of ValueKind.Link:
            env.`<=`(t1.to, t2.to)
        else:
            debug t1.kind
            env.raise("notimplemented")
            false
    # elif t1.kind == ValueKind.Sigma and t2.kind == ValueKind.Sigma:
    #     `<=`(env, t1.first, t2.first) and `<=`(env, t1.second, t2.second)
    elif t1.kind == ValueKind.Intersection:
        t1.types.anyIt(env.`<=`(it, t2))
    elif t2.kind == ValueKind.Intersection:
        t2.types.allIt(env.`<=`(t1, it))
    elif t1.kind == ValueKind.Union:
        t1.types.allIt(env.`<=`(it, t2))
    elif t2.kind == ValueKind.Union:
        t2.types.anyIt(env.`<=`(t1, it))
    elif t2.kind == ValueKind.Distinct:
        env.`<=`(t1, t2.base)
    elif t1.kind == ValueKind.Var:
        env.`<=`(t1.tv.ub, t2)
    elif t2.kind == ValueKind.Var:
        env.`<=`(t1, t2.tv.lb)
    elif t1.kind == ValueKind.Unit:
        false
    elif t2.kind == ValueKind.Bottom:
        false
    else:
        # env.scope.typeOrder.path(t1, t2).isSome
        (t1, t2) in env.scope.typeOrder
proc `<=?`*(env: TypeEnv, t1, t2: Value): Option[seq[Constraint]] =
    proc `<=`(t1, t2: Value): bool =
        env.`<=`(t1, t2)
    if t1 <= t2:
        some newSeq[Constraint]()
    elif t1.kind == t2.kind:
        case t1.kind
        of ValueKind.Bottom, ValueKind.Unit, ValueKind.Integer, ValueKind.Float, ValueKind.Char, ValueKind.String:
            some newSeq[Constraint]()
        of ValueKind.Pair:
            let
                first = env.`<=?`(t1.first, t2.first)
                second = env.`<=?`(t1.second, t2.second)
            if first.isSome and second.isSome:
                some first.get & second.get
            else:
                none(seq[Constraint])
        of ValueKind.Record:
            let
                ret = toSeq(t2.members.keys).mapIt(`<=?`(env, t1.members.getOrDefault(it, Value.Unit), t2.members[it]))
            if ret.allIt(it.isSome):
                some ret.mapIt(it.get).flatten
            else:
                none(seq[Constraint])
        of ValueKind.Pi:
            let
                params = t1.params.zip(t2.params).mapIt(env.`<=?`(it[1], it[0]))
                rety = env.`<=?`(t1.rety, t2.rety)
            if params.allIt(it.isSome) and rety.isSome:
                some params.mapIt(it.get).flatten & rety.get
            else:
                none(seq[Constraint])
        of ValueKind.Var:
            if t1.tv.lb <= t2.tv.ub:
                some @[(t1, t2)]
            else:
                none(seq[Constraint])
        else:
            env.raise("not implemented")
            none(seq[Constraint])
    elif t2.kind == ValueKind.Var:
        # if t2.tv.ub != t1 and t2.tv.ub <= t1:
        #     none(seq[Constraint])
        # else:
        #     some @[(t1, t2)]
        if t1 <= t2.tv.ub:
            some @[(t1, t2)]
        else:
            none(seq[Constraint])
    elif t1.kind == ValueKind.Var:
        # if t2 != t1.tv.lb and t2 <= t1.tv.lb:
        #     none(seq[Constraint])
        # else:
        #     some @[(t1, t2)]
        if t1.tv.lb <= t2:
            some @[(t1, t2)]
        else:
            none(seq[Constraint])
    else:
        if env.scope.typeOrder.path(t1, t2).isSome:
            some newSeq[Constraint]()
        else:
            none(seq[Constraint])

template setTypeEnv*(env: TypeEnv): untyped =
    proc `<=`(t1, t2: Value): bool =
        env.`<=`(t1, t2)
    proc `<=?`(t1, t2: Value): Option[seq[Constraint]] =
        env.`<=?`(t1, t2)
    # proc `==`(t1, t2: Value): bool =
    #     t1 <= t2 and t2 <= t1


proc lub*(self: TypeEnv, t1, t2: Value): Value =
    setTypeEnv(self)
    if t1 <= t2:
        t1
    elif t2 <= t1:
        t2
    else:
        if t1.kind == t2.kind:
            self.raise(fmt"{t1} and {t2} can not be unified")
            Value.Intersection(@[t1, t2])
        elif t1.kind == ValueKind.Union:
            Value.Union(t1.types.filter(it => it <= t2))
        elif t2.kind == ValueKind.Union:
            self.lub(t2, t1)
        else:
            self.raise(fmt"{t1} and {t2} can not be unified")
            Value.Intersection(@[t1, t2])
proc glb*(self: TypeEnv, t1, t2: Value): Value =
    setTypeEnv(self)
    if t1 <= t2:
        t2
    elif t2 <= t1:
        t1
    else:
        if t1.kind == t2.kind:
            self.raise(fmt"{t1} and {t2} can not be unified")
            Value.Union(@[t1, t2])
        elif t1.kind == ValueKind.Intersection:
            Value.Intersection(t1.types.filter(it => t2 <= it))
        elif t2.kind == ValueKind.Intersection:
            self.glb(t2, t1)
        else:
            self.raise(fmt"{t1} and {t2} can not be unified")
            Value.Union(@[t1, t2])

