
import strformat
import sequtils
import sets
import tables
import algorithm
import sugar

import
    il,
    typeenvs,
    errors,
    relations,
    orders,
    lineinfos,
    utils


proc `<=`*(env: TypeEnv, t1, t2: ref Value): bool =
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
        env.`<=`(t1.gen.ub, t2)
    elif t1.kind == t2.kind:
        case t1.kind
        of ValueKind.Bottom:
            true
        of ValueKind.`()`:
            true
        of ValueKind.Unit:
            true
        of ValueKind.U:
            t1.level <= t2.level
        of ValueKind.Integer:
            true
        of ValueKind.Float:
            true
        of ValueKind.Char:
            true
        of ValueKind.String:
            true
        of ValueKind.Pair:
            `<=`(env, t1.first, t2.first) and `<=`(env, t1.second, t2.second)
        of ValueKind.Record:
            toSeq(t2.members.keys).allIt(`<=`(env, t1.members.getOrDefault(it, Value.Unit), t2.members[it]))
        of ValueKind.Pi:
            # TODO: genty
            let
                paramty = t1.paramty.zip(t2.paramty).allIt(env.`<=`(it[1], it[0]))
                rety = env.`<=`(t1.rety, t2.rety)
            paramty and rety
        of ValueKind.Typedesc:
            env.`<=`(t1.`typedesc`, t2.`typedesc`)
        of ValueKind.Var:
            false
        of ValueKind.Link:
            env.`<=`(t1.to, t2.to)
        else:
            echo t1.kind
            raise newException(TypeError, "notimplemented")
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
    elif t1.kind == ValueKind.Var:
        env.`<=`(t1.tv.ub, t2)
    elif t2.kind == ValueKind.Var:
        env.`<=`(t1, t2.tv.lb)
    else:
        env.scope.typeOrder.path(t1, t2).isSome
proc `<=?`*(env: TypeEnv, t1, t2: ref Value): Option[seq[Constraint]] =
    proc `<=`(t1, t2: ref Value): bool =
        env.`<=`(t1, t2)
    if t1 <= t2:
        some newSeq[Constraint]()
    elif t1.kind == t2.kind:
        case t1.kind
        of ValueKind.Bottom, ValueKind.`()`, ValueKind.Unit, ValueKind.U, ValueKind.Integer, ValueKind.Float, ValueKind.Char, ValueKind.String:
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
                paramty = t1.paramty.zip(t2.paramty).mapIt(env.`<=?`(it[1], it[0]))
                rety = env.`<=?`(t1.rety, t2.rety)
            if paramty.allIt(it.isSome) and rety.isSome:
                some paramty.mapIt(it.get).flatten & rety.get
            else:
                none(seq[Constraint])
        of ValueKind.Var:
            if t1.tv.lb <= t2.tv.ub:
                some @[(t1, t2)]
            else:
                none(seq[Constraint])
        else:
            raise newException(TypeError, "not implemented")
            # none(seq[Constraint])
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

template setTypeEnv(env: TypeEnv): untyped =
    template `<=`(t1, t2: ref Value): bool =
        env.`<=`(t1, t2)
    template `<=?`(t1, t2: ref Value): Option[seq[Constraint]] =
        env.`<=?`(t1, t2)

proc lub(self: TypeEnv, t1, t2: ref Value): ref Value =
    setTypeEnv(self)
    if t1 <= t2:
        t1
    elif t2 <= t1:
        t2
    else:
        if t1.kind == t2.kind:
            raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
        elif t1.kind == ValueKind.Union:
            Value.Union(t1.types.filter(it => it <= t2))
        elif t2.kind == ValueKind.Union:
            self.lub(t2, t1)
        else:
            raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
proc glb(self: TypeEnv, t1, t2: ref Value): ref Value =
    setTypeEnv(self)
    if t1 <= t2:
        t2
    elif t2 <= t1:
        t1
    else:
        if t1.kind == t2.kind:
            raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
        elif t1.kind == ValueKind.Intersection:
            Value.Intersection(t1.types.filter(it => t2 <= it))
        elif t2.kind == ValueKind.Intersection:
            self.glb(t2, t1)
        else:
            raise newException(TypeError, fmt"{t1} and {t2} can not be unified")

proc coerceRelation(self: TypeEnv, t1, t2: ref Value) =
    if t1.kind == t2.kind and t1.kind in {ValueKind.Bottom..ValueKind.String}:
        discard
    else:
        self.constraints.add (t1, t2)
proc coerceEq(self: TypeEnv, t1, t2: ref Value) =
    self.coerceRelation(t1, t2)
    self.coerceRelation(t2, t1)
proc addTypeRelation(self: TypeEnv, t1, t2: ref Value, fn: Ident) =
    setTypeEnv(self)
    # assert t1.kind == ValueKind.Typedesc
    # assert t2.kind == ValueKind.Typedesc
    # assert not (t2.`typedesc` <= t1.`typedesc`)
    self.scope.typeOrder.add (t1, t2)
    self.scope.converters[(t1, t2)] = fn

proc bindtv(self: TypeEnv, t1, t2: ref Value) =
    case t2.kind
    of ValueKind.Var:
        let symbol = if t1.symbol.isSome: t1.symbol else: t2.symbol
        t1[] = Value.Link(t2)[]
        t1.symbol = symbol
    of ValueKind.Link:
        self.bindtv(t1, t2.to)
    else:
        if t1.kind in {ValueKind.Intersection, ValueKind.Union}:
            t1[] = t2[]
        else:
            let symbol = if t1.symbol.isSome: t1.symbol else: t2.symbol
            t1[] = t2[]
            t1.symbol = symbol
proc simplify*(self: TypeEnv, t: ref Value): ref Value {.discardable.} =
    setTypeEnv(self)
    result = t
    assert t.kind == ValueKind.Var
    if t.tv.lb.kind == ValueKind.Intersection:
        let
            tmp = t.tv.lb.types.filter(it => it <= t.tv.ub)
        self.bindtv(t.tv.lb, Value.Intersection(tmp))
    if t.tv.ub.kind == ValueKind.Union:
        let
            tmp = t.tv.ub.types.filter(it => t.tv.lb <= it)
        self.bindtv(t.tv.ub, Value.Union(tmp))
    if t.tv.ub <= t.tv.lb:
        self.bindtv(t, t.tv.lb)
proc containtv(self: ref Value): bool =
    case self.kind
    of ValueKind.Bottom, ValueKind.`()`, ValueKind.Unit, ValueKind.U:
        false
    # of ValueKind.# Bool:
    #     false
    of ValueKind.Integer, ValueKind.Float, ValueKind.Char, ValueKind.String:
        false
    # of ValueKind.# List:
    #     false
    of ValueKind.Pair:
        self.first.containtv or self.second.containtv
    # of ValueKind.# Tuple:
    #     false
    of ValueKind.Record:
        toSeq(self.members.values).any(containtv)
    of ValueKind.Pi:
        self.paramty.any(containtv) or self.rety.containtv
    # of ValueKind.# Sigma:
    #     false
    of ValueKind.Typedesc:
        self.`typedesc`.containtv
    # of ValueKind.# Distinct:
    #     false
    of ValueKind.Var:
        true
    of ValueKind.Intersection, ValueKind.Union:
        self.types.any(containtv)
    of ValueKind.Link:
        self.to.containtv
    of ValueKind.Gen:
        false
const
    cvar = 1000
    undeciable = 10000
proc likelihoodimpl*(self: TypeEnv, t1, t2: ref Value): int =
    setTypeEnv(self)
    if t1 == t2:
        0
    elif t1.kind == t2.kind:
        case t1.kind
        of ValueKind.Var:
            undeciable
        of ValueKind.Pi:
            var ret = 0
            if t1.paramty.len != t2.paramty.len:
                undeciable
            else:
                for (t1, t2) in t1.paramty.zip(t2.paramty):
                    ret = self.likelihoodimpl(t2, t1)
                ret += self.likelihoodimpl(t1.rety, t2.rety)
                ret
        else:
            echo t1
            echo t2
            assert false, "notimplemented"
            0
    elif t1.kind == ValueKind.Var:
        if t1.tv.ub <= t2:
            cvar
        elif t1.tv.lb <= t2:
            cvar + 10
        else:
            undeciable
    elif t2.kind == ValueKind.Var:
        if t1 <= t2.tv.lb:
            cvar
        elif t1 <= t2.tv.ub:
            cvar + 10
        else:
            undeciable
    elif t1 <= t2:
        let
            tmp = self.scope.typeOrder.path(t1, t2)
        tmp.get(@[]).len * 10
    else:
        echo t1
        echo t2
        assert false, "notimplemented"
        0
proc likelihood*(self: TypeEnv, t1, t2: ref Value): seq[(ref Value, int)] =
    assert t1.kind == ValueKind.Intersection
    assert t2.kind != ValueKind.Intersection
    t1.types.mapIt((it, self.likelihoodimpl(it, t2)))
proc resolveRelation(self: TypeEnv, t1, t2: ref Value) =
    # implies t1 < t2
    # setTypeEnv(self)
    setTypeEnv(self)
    # TODO: should change function name
    if t1.kind == ValueKind.Link:
        self.resolveRelation(t1.to, t2)
    elif t2.kind == ValueKind.Link:
        self.resolveRelation(t1, t2.to)
    elif t1 == t2:
        return
    # elif t1.kind == ValueKind.Gen:
    #     self.resolveRelation(t1.gen.ub, t2)
    # elif t2.kind == ValueKind.Gen:
    #     self.resolveRelation(t1, t2.gen.ub)
    elif t1.kind == t2.kind:
        case t1.kind
        of ValueKind.Bottom..ValueKind.String:
            return
        # of ValueKind.List:
        #     self.resolveRelation(t1.base, t2.base)
        of ValueKind.Pair:
            self.resolveRelation(t1.first, t2.first)
            self.resolveRelation(t1.second, t2.second)
        of ValueKind.Record:
            for member in t2.members.keys:
                self.resolveRelation(t1.members[member], t2.members[member])
        # of ValueKind.Sigma:
        #     self.resolveRelation(t1.first, t2.first)
        #     self.resolveRelation(t1.second, t2.second)
        # of ValueKind.Tuple:
        #     if t1.types.len == t2.types.len:
        #         for (t1, t2) in t1.types.zip(t2.types):
        #             self.resolveRelation(t1, t2)
        #     else:
        #         raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
        # of ValueKind.Record:
        #     # TODO: implment for record
        #     raise newException(TypeError, "notimplemented")
        of ValueKind.Pi:
            for (t1, t2) in t1.paramty.zip(t2.paramty):
                self.resolveRelation(t2, t1)
            self.resolveRelation(t1.rety, t2.rety)
        of ValueKind.Typedesc:
            self.resolveRelation(t1.`typedesc`, t2.`typedesc`)
        of ValueKind.Var:
            t1.tv.ub = self.lub(t1.tv.ub, t2.tv.ub)
            t2.tv.lb = self.glb(t1.tv.lb, t2.tv.lb)
            self.simplify(t1)
            self.simplify(t2)
            if t1.kind == ValueKind.Var and t2.kind == ValueKind.Var:
                self.tvconstraints.add (t1, t2)
        else:
            echo t1
            echo t2
            echo $t1.symbol
            echo t2.symbol
            echo self.constraints
            raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
    else:
        if t1.kind == ValueKind.Var:
            # TODO: if lb >= t2
            # if t1.tv.lb <= t2 and t2 <= t1.tv.ub:
            #     t1.tv.ub = t2
            if t1.tv.lb <= t2:
                t1.tv.ub = self.lub(t1.tv.ub, t2)
            # elif t1.tv.ub <= t2:
            #     discard
            else:
                raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
            self.simplify(t1)
        elif t2.kind == ValueKind.Var:
            # if t2.tv.lb <= t1 and t1 <= t2.tv.ub:
            #     t2.tv.lb = t1
            if t1 <= t2.tv.ub:
                t2.tv.lb = self.glb(t1, t2.tv.lb)
            # elif t1 <= t2.tv.lb:
            #     discard
            else:
                echo t1
                echo t2.tv.ub
                echo t1 <= t2.tv.ub
                raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
            self.simplify(t2)
        elif t1.kind == ValueKind.Intersection:
            let
                tmp = toSeq(t1.types.items)
                l = tmp.zip(tmp.mapIt(it <=? t2)).filterIt(it[1].isSome).mapIt((it[0], it[1].get))
            case l.len
            of 0:
                raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
            of 1:
                let (t3, cons) = l[0]
                self.bindtv(t1, t3)
                self.constraints.add cons
                self.constraints.add (t1, t2)
                # TODO: add opppsite constraint
                # if t3.kind == ValueKind.Arrow and t2.kind == ValueKind.Arrow:
                #     self.constraints.add (t2.rety, t3.rety)
                if t3.kind == ValueKind.Pi and t2.kind == ValueKind.Pi:
                    self.constraints.add (t2.rety, t3.rety)
            else:
                self.bindtv(t1, Value.Intersection(l.mapIt(it[0])))
                if t2.kind == ValueKind.Pi:
                    let
                        tmp = Value.Intersection(t1.types.map(it => it.rety))
                    if not (tmp <= t2.rety):
                        self.interconstraints.add (tmp, t2.rety)
                let
                    likeli = self.likelihood(t1, t2)
                if likeli.anyIt(it[1] >= cvar):
                    self.interconstraints.add (t1, t2)
                else:
                    let
                        min = likeli.filterIt(it[1] == likeli.mapIt(it[1]).min)
                    if min.len == 1:
                        self.bindtv(t1, min[0][0])
                    else:
                        self.interconstraints.add (t1, t2)
        elif t2.kind == ValueKind.Union:
            let
                tmp = toSeq(t2.types.items)
                l = tmp.zip(tmp.mapIt(t1 <=? it)).filterIt(it[1].isSome).mapIt((it[0], it[1].get))
            case l.len
            of 0:
                raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
            of 1:
                let (t3, cons) = l[0]
                self.bindtv(t2, t3)
                self.constraints.add cons
                self.constraints.add (t1, t2)
                # TODO: add opppsite constraint
                # if t3.kind == ValueKind.Arrow and t2.kind == ValueKind.Arrow:
                #     self.constraints.add (t2.rety, t3.rety)
                if t3.kind == ValueKind.Pi and t1.kind == ValueKind.Pi:
                    self.constraints.add (t1.rety, t3.rety)
            else:
                self.bindtv(t2, Value.Union(l.mapIt(it[0])))
                self.interconstraints.add (t1, t2)
        # elif t2.kind == ValueKind.Tuple:
        #     case t2.types.len
        #     of 0:
        #         self.constraints.add (t1, Value.Unit)
        #     of 1:
        #         self.constraints.add (t1, Value.Pair(t2.types[0], Value.Unit))
        #     of 2:
        #         self.constraints.add (t1, Value.Pair(t2.types[0], t2.types[1]))
        #     else:
        #         self.constraints.add (t1, Value.Pair(t2.types[0], Value.Tuple(t2.types[1..^1])))
        # elif t1.kind == ValueKind.Tuple:
        #     case t1.types.len
        #     of 0:
        #         self.constraints.add (Value.Unit, t2)
        #     of 1:
        #         self.constraints.add (Value.Pair(t1.types[0], Value.Unit), t2)
        #     of 2:
        #         self.constraints.add (Value.Pair(t1.types[0], t1.types[1]), t2)
        #     else:
        #         self.constraints.add (Value.Pair(t1.types[0], Value.Tuple(t1.types[1..^1])), t2)
        elif t1 <= t2:
            discard
        else:
            echo t1
            echo t2
            echo t1.symbol
            echo t2.symbol
            echo t1.kind
            echo t2.kind
            echo self.constraints
            raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
proc resolveRelations(self: TypeEnv) =
    var tmp: seq[Constraint]
    while self.constraints.len > 0:
        self.constraints.reverse
        while self.constraints.len > 0:
            let
                (t1, t2) = self.constraints.pop
            self.resolveRelation(t1, t2)
        var
            ord = newOrder[ref Value]()
            f = false
        if self.tvconstraints.filterIt(it[0].kind != ValueKind.Var or it[1].kind != ValueKind.Var).len != 0:
            self.constraints = self.tvconstraints & self.interconstraints
            self.tvconstraints = @[]
            self.interconstraints = @[]
            continue
        for cons in self.tvconstraints:
            let
                (t1, t2) = cons
                path = ord.path(t2, t1)
            if path.isSome:
                for (t1, t2) in path.get:
                    self.resolveRelation(t1, t2)
                self.resolveRelation(t1, t2)
                for t2 in path.get.mapIt(it[0]):
                    self.bindtv(t2, t1)
                self.constraints = self.tvconstraints & self.interconstraints
                self.tvconstraints = @[]
                self.interconstraints = @[]
                f = true
                break
            else:
                ord.add cons
        if f: continue
        self.tvs = self.tvs.filter(it => it.kind == ValueKind.Var).map(it => self.simplify(it))
        for e in self.tvs:
            if e notin ord.primal and e notin ord.dual:
                if e.tv.lb.compilable:
                    self.bindtv(e, e.tv.lb)
                    f = true
        if f:
            self.constraints = self.tvconstraints & self.interconstraints
            self.tvconstraints = @[]
            self.interconstraints = @[]
            continue
        for e in ord.sort:
            if e.tv.lb.compilable:
                self.bindtv(e, e.tv.lb)
            else:
                break

        self.constraints = self.tvconstraints & self.interconstraints
        self.tvconstraints = @[]
        self.interconstraints = @[]
        if self.constraints.len == 0:
            break
        if self.constraints.len == tmp.len and self.constraints.zip(tmp).allIt(it[0] == it[1]):
            break
        tmp = self.constraints

proc typeInfer*(self: Term, env: TypeEnv, global: bool = false): ref Value
proc evalConst*(self: Term, env: TypeEnv, global: bool = false): ref Value =
    case self.kind
    of TermKind.`()`:
        Value.unit
    of TermKind.Unit:
        Value.Unit
    of TermKind.U:
        Value.U
    of TermKind.Id:
        let
            syms = env.lookupConst(self.name)
        case syms.len
        of 0:
            Value.Var(env)
        of 1:
            syms[0].typ
        else:
            Value.Intersection(syms.mapIt(it.typ))
    of TermKind.Tuple:
        case self.terms.len
        of 0:
            Value.unit()
        of 1:
            Value.Pair(self.terms[0].evalConst(env, global), Value.unit)
        of 2:
            Value.Pair(self.terms[0].evalConst(env, global), self.terms[1].evalConst(env, global))
        else:
            Value.Pair(self.terms[0].evalConst(env, global), Term.Tuple(self.terms[1..^1]).evalConst(env, global))
    of TermKind.Record:
        Value.Record(toSeq(self.members.pairs).mapIt((it[0], it[1].evalConst(env, global))).toTable)
    of TermKind.Typeof:
        self.term.typeInfer(env, global)
    else:
        echo self
        raise newException(TypeError, "")
proc typeInfer(self: Pattern, env: TypeEnv, global: bool = false): ref Value =
    result = case self.kind
    of PatternKind.Literal:
        self.lit.typeInfer(env, global)
    of PatternKind.Ident:
        let
            tv = Value.Var(env)
        self.id.typ = tv
        tv
    of PatternKind.Pair:
        Value.Pair(self.first.typeInfer(env, global), self.second.typeInfer(env, global))
    of PatternKind.Record:
        Value.Record(toSeq(self.members.pairs).mapIt((it[0], it[1].typeInfer(env, global))).toTable)
    of PatternKind.Discard:
        Value.Var(env)
    self.typ = result
proc addPatConst(self: TypeEnv, pat: Pattern, constval: ref Value, impl: Term, global: bool) =
    case pat.kind
    of PatternKind.Literal:
        assert constval.typ == pat.lit.typeInfer(self, global)
        pat.typ = constval.typ
    of PatternKind.Ident:
        let
            t = constval.typ
            sym = if t.typ.kind == ValueKind.U:
                Symbol.Typ(pat.id, t, impl, global)
            else:
                Symbol.Const(pat.id, t, impl, global)
            con = if t.typ.kind == ValueKind.U:
                Symbol.Typ(pat.id, constval, impl, global)
            else:
                Symbol.Const(pat.id, constval, impl, global)
        # pat.id.typ = t.typ # ?
        # pat.typ = t.typ
        pat.id.typ = t
        pat.typ = t
        self.addIdent(pat.id, sym)
        self.addConst(pat.id, con)
    of PatternKind.Pair:
        assert constval.kind == ValueKind.Pair
        self.addPatConst(pat.first, constval.first, impl, global)
        self.addPatConst(pat.second, constval.first, impl, global)
        pat.typ = constval.typ
    of PatternKind.Record:
        assert constval.kind == ValueKind.Record
        for key in pat.members.keys:
            self.addPatConst(pat.members[key], constval.members[key], impl, global)
    of PatternKind.Discard:
        pat.typ = constval.typ
proc typeInfer*(self: Term, env: TypeEnv, global: bool = false): ref Value =
    if not self.typ.isNil:
        return self.typ
    proc addPatFuncDef(self: TypeEnv, pat: Pattern, t: ref Value, impl: Term, global: bool) =
            case pat.kind
            of PatternKind.Literal:
                assert t == pat.lit.typeInfer(self, global)
            of PatternKind.Ident:
                let
                    sym = Symbol.Let(pat.id, t, impl, global)
                pat.id.typ = t
                self.addIdent(pat.id, sym)
            of PatternKind.Pair:
                assert t.kind == ValueKind.Pair
                self.addPatFuncDef(pat.first, t.first, impl, global)
                self.addPatFuncDef(pat.second, t.second, impl, global)
            of PatternKind.Record:
                assert t.kind == ValueKind.Record
                for key in pat.members.keys:
                    self.addPatFuncDef(pat.members[key], t.members[key], impl, global)
            of PatternKind.Discard:
                discard
            pat.typ = t
    result = case self.kind
    of TermKind.Failed:
        Value.Unit
    of TermKind.bottom:
        Value.Bottom
    of TermKind.`()`:
        Value.Unit
    of TermKind.Unit:
        Value.U
    of TermKind.U:
        Value.U(1)
    # of TermKind.Bool:
    #     Value.Bool
    of TermKind.Integer:
        Value.Integer
    of TermKind.Float:
        Value.Float
    of TermKind.Char:
        Value.Char
    of TermKind.String:
        Value.String
    of TermKind.Id:
        let
            syms = env.lookupId(self.name)
        case syms.len
        of 0:
            Value.Var(env)
        of 1:
            syms[0].typ.inst(env)
        else:
            Value.Intersection(syms.mapIt(it.typ.inst(env)))
    # of TermKind.Lambda:
    #     Value.Unit
    # of TermKind.List:
    #     Value.Unit
    of TermKind.Tuple:
        case self.terms.len
        of 0:
            Value.Unit
        of 1:
            Value.Pair(self.terms[0].typeInfer(env, global), Value.Unit)
            # Value.Sigma(self.terms[0].typeInfer(env, global), Value.Unit)
        else:
            self.terms.mapIt(it.typeInfer(env, global)).foldr(Value.Pair(a, b))
            # self.terms.mapIt(it.typeInfer(env, global)).foldr(Value.Sigma(a, b))
        # Value.Tuple(self.seqval.mapIt(it.typeInfer(env, global)))
    of TermKind.Record:
        Value.Record(toSeq(self.members.pairs).mapIt((it[0], it[1].typeInfer(env, global))).toTable)
    of TermKind.Let:
        proc addPat(self: TypeEnv, pat: Pattern, impl: Term, global: bool) =
            case pat.kind
            of PatternKind.Literal:
                discard
            of PatternKind.Ident:
                let
                    sym = Symbol.Let(pat.id, pat.id.typ, impl, global)
                self.addIdent(pat.id, sym)
            of PatternKind.Pair:
                self.addPat(pat.first, impl, global)
                self.addPat(pat.second, impl, global)
            of PatternKind.Record:
                for pat in pat.members.values:
                    self.addPat(pat, impl, global)
            of PatternKind.Discard:
                discard
        let
            pat = self.iddef.pat
            tv = pat.typeInfer(env, global)
            impl = self.iddef.default.get
            t1 = impl.typeInfer(env, global)
        if self.iddef.typ.isSome:
            let
                _ = self.iddef.typ.get.typeInfer(env, global)
                t = self.iddef.typ.get.evalConst(env, global)
            env.coerceEq(tv, t)
        if t1.kind == ValueKind.Intersection:
            env.coerceRelation(t1, tv)
            env.coerceRelation(tv, Value.Union(t1.types))
        else:
            if self.iddef.typ.isSome:
                env.coerceRelation(t1, tv)
            else:
                env.coerceEq(tv, t1)
        env.addPat(pat, impl, global)
        Value.Unit
    # of TermKind.Var:
    #     let
    #         tv = Value.Var
    #         id = self.iddef.id
    #         impl = self.iddef.default.get
    #         t1 = impl.typeInfer(env, global)
    #         sym = if t1.kind == ValueKind.Arrow:
    #             # PSymbol.Let(t1.gen(env), self, global)
    #             # TODO:
    #             PSymbol()
    #         else:
    #             PSymbol.Let(id, PolyType.ForAll(nullFtv, tv), impl, global)
    #         # sym = PSymbol.Let(t1.gen(env))
    #     if self.iddef.typ.isSome:
    #         let
    #             typ = self.iddef.typ.get.typeInfer(env, global)
    #         env.coerceRelation(Value.Typedesc(tv), typ)
    #         env.coerceRelation(typ, Value.Typedesc(tv))
    #     env.coerceRelation(t1, tv)
    #     id.typ = tv
    #     env.addIdent(id, sym)
    #     Value.Unit
    of TermKind.Const:
        let
            iddef = self.iddef
            pat = iddef.pat
            default = iddef.default.get
            _ = default.typeInfer(env, global)
            val = default.evalConst(env)
        env.addPatConst(pat, val, default, global)
        Value.Unit
    of TermKind.Funcdef, TermKind.FuncdefInst:
        let
            genty = self.fn.param.gen.mapIt((it.pat, Value.Gen(it.pat.id.name, it.default.get.evalConst(env, global), it.typ.get.evalConst(env, global))))
            # tvs = self.fn.param.params.mapIt(Value.Var())
            # tv = Value.Var()
            # fnty = Value.Arrow(tvs, tv)
            # fnty = Value.Pi(genty, tvs, tv)
            # sym = Symbol.Func(self.fn.id, fnty, self, global)
        # self.fn.id.typ = fnty
        # env.addIdent(self.fn.id, sym)
        env.pushScope self.fn.body.scope
        for (gen, impl) in genty.zip(self.fn.param.gen):
            let (pat, val) = gen
            env.addPatConst(pat, val, impl.default.get, false)
        let
            _ = self.fn.param.params.mapIt(it.typ.get.typeInfer(env, false))
            _ = self.fn.param.rety.typeInfer(env, global)
            paramty = self.fn.param.params.mapIt(it.typ.get.evalConst(env, false))
            rety = self.fn.param.rety.evalConst(env, false)
            metadata = self.fn.metadata
            tvs = paramty.mapIt(Value.Var(env))
            tv = Value.Var(env)
            # fnty = Value.Pi(genty, paramty, rety)
            fnty = Value.Pi(genty, tvs, tv)
            sym = Symbol.Func(self.fn.id, fnty, self, global)
        # for (p, v) in paramty.zip(tvs):
        #     env.coerceEq(v, p)
        # env.coerceEq(tv, rety)
        # env.resolveRelations()
        # instead of the above
        for (p, v) in paramty.zip(tvs):
            env.bindtv(v, p)
        env.bindtv(tv, rety)
        # for (iddef, t) in self.fn.param.params.zip(paramty):
        #     env.addPatFuncDef(iddef.pat, t, iddef.default.get(Term.unit), global)
        for (iddef, t) in self.fn.param.params.zip(tvs):
            env.addPatFuncDef(iddef.pat, t, iddef.default.get(Term.unit), global)
        let
            inferedRety = self.fn.body.term.typeInfer(env)
        env.popScope
        if metadata.isSome:
            case metadata.get.kind
            of MetadataKind.ImportLL:
                env.coerceEq(inferedRety, Value.Unit)
            of MetadataKind.Subtype:
                assert paramty.len == 1, "converter must take only one argument."
                env.addTypeRelation(paramty[0], rety, self.fn.id)
                env.coerceRelation(inferedRety, rety)
            else:
                discard
        else:
            env.coerceRelation(inferedRety, rety)
        self.fn.id.typ = fnty
        if self.kind == TermKind.Funcdef:
            env.addIdent(self.fn.id, sym)
        Value.Unit
    # of TermKind.If:
    #     let
    #         conds = self.`elif`.mapIt(it[0].typeInfer(env, global))
    #         thents = self.`elif`.mapIt(
    #             block:
    #                 env.pushScope it[1].scope
    #                 let body = it[1].term.typeInfer(env, global)
    #                 env.popScope
    #                 body
    #             )
    #         elset = block:
    #             env.pushScope self.`else`.scope
    #             let body = self.`else`.term.typeInfer(env, global)
    #             env.popScope
    #             body
    #         tv = Value.Var
    #     for cond in conds:
    #         env.coerceRelation(cond, Value.Bool)
    #         env.coerceRelation(Value.Bool, cond)
    #     for thent in thents:
    #         env.coerceRelation(thent, tv)
    #     env.coerceRelation(elset, tv)
    #     tv
    # of TermKind.When:
    #     Value.Unit
    of TermKind.Case:
        Value.Unit
    # of TermKind.While:
    #     Value.Unit
    # of TermKind.For:
    #     Value.Unit
    # of TermKind.Loop:
    #     Value.Unit
    # of TermKind.Block:
    #     Value.Unit
    # of TermKind.Asign:
    #     let
    #         pat = self.pat.typeInfer(env, global)
    #         val = self.val.typeInfer(env, global)
    #     env.coerceRelation(val, pat)
    #     Value.Unit
    of TermKind.Typeof:
        Value.Typedesc(self.term.typeInfer(env, global))
    of TermKind.Discard:
        discard self.term.typeInfer(env)
        Value.Unit
    of TermKind.Apply:
        let
            callee = self.callee.typeInfer(env, global)
            args = self.args.mapIt(it.typeInfer(env, global))
            tv = Value.Var(env)
        env.coerceRelation(callee, Value.Arrow(args, tv))
        if callee.kind == ValueKind.Intersection:
            env.coerceRelation(tv, Value.Union(callee.types.filter(it => it.kind == ValueKind.Pi).map(it => it.rety)))
        else:
            env.coerceRelation(tv, callee.rety)
        tv
    # of TermKind.Projection:
    #     let
    #         container = self.container.typeInfer(env, global)
    #         index = self.index
    #         typ = Value.Pair(Value.Var, Value.Var)
    #         # typ = Value.Sigma(Value.Var, Value.Var)
    #     env.coerceEq(container, typ)
    #     if index == 0:
    #         typ.first
    #     else:
    #         typ.second
    of TermKind.Meta:
        discard self.metadata.param.mapIt(it.typeInfer(env, global))
        Value.Unit
    of TermKind.Seq:
        let
            terms = self.terms.mapIt(it.typeInfer(env, global))
        for e in terms[0..^2]:
            env.coerceEq(e, Value.Unit)
        terms[^1]
    self.typ = result

proc typeCheck(self: Term, env: TypeEnv, gen: bool = false): seq[Error]
proc typeCheck(self: Pattern, env: TypeEnv): seq[Error] =
    case self.kind
    of PatternKind.Literal:
        self.lit.typeCheck(env)
    of PatternKind.Ident:
        self.id.typeCheck(env)
    of PatternKind.Pair:
        self.first.typeCheck(env) & self.second.typeCheck(env)
    of PatternKind.Record:
        toSeq(self.members.values).mapIt(it.typeCheck(env)).flatten
    of PatternKind.Discard:
        @[]
proc typeCheck(self: Term, env: TypeEnv, gen: bool = false): seq[Error] =
    setTypeEnv(env)
    if self.typ.kind == ValueKind.Link:
        # env.bindtv(self.typ, self.typ.to)
        let symbol = if self.typ.symbol.isSome: self.typ.symbol else: self.typ.to.symbol
        self.typ[] = self.typ.to[]
        self.typ.symbol = symbol
        return self.typeCheck(env)
    if self.typ.kind == ValueKind.Var:
        if self.typ.tv.lb.compilable:
            env.bindtv(self.typ, self.typ.tv.lb)
        elif self.kind == TermKind.Id and self.typ.symbol.isNone:
            return @[SemaError.Undefined(self.name)]
        else:
            return @[TypeError.Undeciable($self & " at " & $self.loc)]
    elif self.typ.kind == ValueKind.Intersection:
        return @[TypeError.Undeciable($self)]
    proc check(self: Term, typ2: ref Value): seq[Error] =
        if self.typ == typ2:
            @[]
        else:
            @[InternalError.new]
    case self.kind
    of TermKind.Failed:
        self.check(Value.Unit)
    of TermKind.bottom:
        self.check(Value.Bottom)
    of TermKind.`()`:
        self.check(Value.Unit)
    of TermKind.Unit:
        self.check(Value.U)
    of TermKind.U:
        self.check(Value.U(1))
    # of TermKind.Bool:
    #     self.check(Value.Bool)
    of TermKind.Integer:
        self.check(Value.Integer)
    of TermKind.Float:
        self.check(Value.Float)
    of TermKind.Char:
        self.check(Value.Char)
    of TermKind.String:
        self.check(Value.String)
    of TermKind.Id:
        if self.typ.symbol.isSome:
            @[]
        else:
            @[SemaError.Undefined(self.name)]
    # of TermKind.Lambda:
    #     self.check(Value.Unit)
    # of TermKind.List:
    #     self.check(Value.Unit)
    of TermKind.Tuple:
        # self.seqval.mapIt(it.typeCheck(env)).flatten & self.check(Value.Tuple(self.seqval.mapIt(it.typ)))
        self.terms.mapIt(it.typeCheck(env)).flatten & self.check(self.terms.mapIt(it.typ).foldr(Value.Pair(a, b)))
        # self.terms.mapIt(it.typeCheck(env)).flatten & self.check(self.terms.mapIt(it.typ).foldr(Value.Sigma(a, b)))
    of TermKind.Record:
        toSeq(self.members.values).mapIt(it.typeCheck(env)).flatten & self.check(Value.Record(toSeq(self.members.pairs).mapIt((it[0], it[1].typ)).toTable))
    of TermKind.Let:
        let
            pat = self.iddef.pat
            impl = self.iddef.default.get
            ret = pat.typeCheck(env) & impl.typeCheck(env)
        if self.iddef.typ.isSome:
            assert impl.typ <= pat.typ, fmt"{impl.typ} <= {pat.typ}"
            var imp = impl
            if impl.typ != pat.typ and impl.typ <= pat.typ and (not gen):
                let
                    p = env.scope.typeOrder.path(impl.typ, pat.typ).get
                for (t1, t2) in p:
                    let
                        fn = env.lookupConverter(t1, t2)
                    imp = Term.Apply(fn, @[imp])
                    imp.typ = t2
                self.iddef.default = some imp
        else:
            assert impl.typ == pat.typ, fmt"{impl.typ} == {pat.typ}"
        self.check(Value.Unit) & ret
    # of TermKind.Var:
    #     let
    #         id = self.iddef.id
    #         impl = self.iddef.default.get
    #         ret = id.typeCheck(env) & impl.typeCheck(env)
    #     assert impl.typ <= id.typ, fmt"{impl.typ} <= {id.typ}"
    #     if impl.typ != id.typ and impl.typ <= id.typ:
    #         let
    #             fn = env.lookupConverter(impl.typ, id.typ)
    #             impl = Term.Apply(fn, @[impl])
    #         impl.typ = id.typ
    #         self.iddef.default = some impl
    #     self.check(Value.Unit) & ret
    of TermKind.Const:
        self.check(Value.Unit)
    # of TermKind.Typedef:
    #     # for typedef in self.typedefs:
    #     #     let
    #     #         id = typedef.id
    #     #         default = typedef.default.get
    #     #         t1 = default.typeInfer(env, global)
    #     #         sym = PSymbol.Typ(id, PolyType.ForAll(nullFtv, t1), default, global)
    #     #     id.typ = t1
    #     #     env.addIdent(id, sym)
    #     self.check(Value.Unit)
    of TermKind.Funcdef, TermKind.FuncdefInst:
        var
            # paramty = self.fn.param.params.mapIt(it.typ.get.typeInfer(env, global))
            # rety = self.fn.param.rety.typeInfer(env, global)
            gen = self.fn.param.gen.len > 0
            ret = self.fn.param.params.mapIt(it.typ.get.typeCheck(env)).flatten &
                self.fn.param.params.mapIt(it.pat.typeCheck(env)).flatten &
                self.fn.id.typeCheck(env) & self.fn.param.rety.typeCheck(env)
        env.pushScope self.fn.body.scope
        ret.add self.fn.body.term.typeCheck(env, gen)
        env.popScope
        #     metadata = self.fn.metadata
        #     tvs = paramty.mapIt(Value.Var())
        #     tv = Value.Var()
        #     fnty = Value.Arrow(tvs, tv)
        #     sym = PSymbol.Func(self.fn.id, PolyType.ForAll(nullFtv, fnty), self, global)
        # env.addIdent(self.fn.id, sym)
        # for (p, v) in paramty.zip(tvs.mapIt(Value.Typedesc(it))):
        #     env.coerceRelation(v, p)
        #     env.coerceRelation(p, v)
        # if rety.kind == ValueKind.Unit:
        #     env.coerceRelation(tv, rety)
        #     env.coerceRelation(rety, tv)
        # else:
        #     env.coerceRelation(Value.TypeDesc(tv), rety)
        #     env.coerceRelation(rety, Value.TypeDesc(tv))
        # env.pushScope
        # for (iddef, tv) in self.fn.param.params.zip(tvs):
        #     let
        #         sym = PSymbol.Let(iddef.id, PolyType.ForAll(nullFtv, tv), iddef.default.get(Term.Unit), global)
        #     iddef.id.typ = tv
        #     env.addIdent(iddef.id, sym)
        # let
        #     inferedRety = self.fn.body.typeInfer(env)
        # env.popScope
        # if metadata.isSome:
        #     case metadata.get.kind
        #     of MetadataKind.ImportLL:
        #         env.coerceRelation(inferedRety, Value.Unit)
        #         env.coerceRelation(Value.Unit, inferedRety)  # ?
        #     of MetadataKind.Subtype:
        #         assert paramty.len == 1, "converter must take only one argument."
        #         env.addTypeRelation(paramty[0], rety)
        #     else:
        #         discard
        # else:
        #     env.coerceRelation(inferedRety, tv)
        self.check(Value.Unit) & ret
    # of TermKind.If:
    #     let
    #         conds = self.`elif`.mapIt(it[0].typeCheck(env))
    #         thens = self.`elif`.mapIt(
    #             block:
    #                 env.pushScope it[1].scope
    #                 let res = it[1].term.typeCheck(env)
    #                 env.popScope
    #                 res
    #         )
    #         elset = block:
    #             env.pushScope self.`else`.scope
    #             let res = self.`else`.term.typeCheck(env)
    #             env.popScope
    #             res
    #         condtypes = self.`elif`.mapIt(it[0].typ)
    #         thentypes = self.`elif`.mapIt(it[1].term.typ)
    #         elsetype = self.`else`.term.typ
    #     for cond in condtypes:
    #         assert cond == Value.Bool
    #     # for thent in thents:
    #     #     env.coerceRelation(thent, tv)
    #     # env.coerceRelation(elset, tv)
    #     # tv
    #     conds.flatten & thens.flatten & elset
    # of TermKind.When:
    #     self.check(Value.Unit)
    of TermKind.Case:
        self.check(Value.Unit)
    # of TermKind.While:
    #     self.check(Value.Unit)
    # of TermKind.For:
    #     self.check(Value.Unit)
    # of TermKind.Loop:
    #     self.check(Value.Unit)
    # of TermKind.Block:
    #     self.check(Value.Unit)
    # of TermKind.Asign:
    #     let
    #         ret = self.pat.typeCheck(env) & self.val.typeCheck(env)
    #     assert self.val.typ <= self.pat.typ, fmt"{self.val.typ} <= {self.pat.typ}"
    #     if self.val.typ != self.pat.typ and self.val.typ <= self.pat.typ:
    #         let
    #             fn = env.lookupConverter(self.val.typ, self.pat.typ)
    #             val = Term.Apply(fn, @[self.val])
    #         val.typ = self.pat.typ
    #         self.val = val
    #     self.check(Value.Unit) & ret
    of TermKind.Typeof:
        self.check(Value.Typedesc(self.term.typ)) & self.term.typeCheck(env)
    of TermKind.Discard:
        self.check(Value.Unit) & self.term.typeCheck(env)
    of TermKind.Apply:
        var ret = self.callee.typeCheck(env) &  self.args.mapIt(it.typeCheck(env)).flatten
        let
            calleety = self.callee.typ
            args = self.args.mapIt(it.typ)
            genty = self.callee.genty
        if genty.len > 0:
            if calleety notin calleety.symbol.get.instances:
                let
                    inst = self.callee.implInst
                    scope = env.scope
                env.scope = inst.fn.body.scope
                for i in 0..<genty.len:
                    let
                        iddef = self.callee.impl.fn.param.gen[i]
                        impl = iddef.default.get
                        pat = iddef.pat
                        val = genty[i]
                    env.addPatConst(pat, val, impl, false)
                env.scope = scope
                discard inst.typeInfer(env)
                env.resolveRelations()
                ret &= inst.typeCheck(env) # must be a empty list
                calleety.symbol.get.instances[calleety] = Impl(instance: some(inst))
        if calleety.kind == ValueKind.Pi:
            for i in 0..<calleety.paramty.len:
                let (argty, paramty) = (args[i], calleety.paramty[i])
                assert argty <= paramty, fmt"{argty} <= {paramty}"
                if argty != paramty and argty <= paramty:
                    let
                        p = env.scope.typeOrder.path(argty, paramty).get
                    var
                        arg = self.args[i]
                    for (t1, t2) in p:
                        let
                            fn = env.lookupConverter(t1, t2)
                        arg = Term.Apply(fn, @[arg])
                        arg.typ = t2
                    self.args[i] = arg
        ret
    # of TermKind.Projection:
    #     let
    #         ret = self.container.typeCheck(env)
    #         container = self.container.typ
    #         index = self.index
    #     # assert container.kind == ValueKind.Tuple
    #     assert container.kind == ValueKind.Pair
    #     # assert container.kind == ValueKind.Sigma
    #     ret & (if index == 0:
    #         self.check(container.first)
    #     else:
    #         self.check(container.second))
    of TermKind.Meta:
        # if not self.metadata.param.isNil:
        #     discard self.metadata.param.typeInfer(env, global)
        self.check(Value.Unit)
    of TermKind.Seq:
        if self.terms.len > 1:
            var ret = self.terms.mapIt(it.typeCheck(env, gen)).flatten
            var check: bool = true
            let
                types = self.terms.mapIt(it.typ)
                typ = types[^1]
            check = types[0..^2].mapIt(it == Value.Unit).foldl(a and b)
            check = check and self.typ.kind != ValueKind.Var and typ.kind != ValueKind.Var
            # self.typ == typ
            ret
        else:
            var
                ret = self.terms[0].typeCheck(env, gen)
            let
                typ = self.terms[0].typ
            ret.add if self.typ.kind != ValueKind.Var and typ.kind != ValueKind.Var:
                @[]
            else:
                @[TypeError.Undeciable]
            ret


proc infer*(self: Term, env: TypeEnv, global: bool = false): (ref Value, seq[Error]) =
    result[0] = self.typeInfer(env, global)
    env.coerceRelation(result[0], Value.Integer)
    env.resolveRelations()
    result[1] = self.typeCheck(env)
