
import sequtils
import options
import sets
import tables
import strformat
import sugar
import algorithm

import ../il
import ../typeenv
import ../orders
import ../errors
import ../utils


proc coerceRelation*(self: TypeEnv, t1, t2: Value) =
    # coerce the relation for t1 <= t2
    if t1.kind == t2.kind and t1.kind in {ValueKind.Bottom..ValueKind.Bool, ValueKind.Char, ValueKind.String}:
        discard
    elif t1.kind == t2.kind and t1.kind in {ValueKind.Integer, ValueKind.Float} and t1.bits == t2.bits:
        discard
    else:
        self.constraints.add (t1, t2)
proc coerceEq*(self: TypeEnv, t1, t2: Value) =
    self.coerceRelation(t1, t2)
    self.coerceRelation(t2, t1)

import macros
macro coerce*(self: TypeEnv, rel: untyped): untyped =
    rel.expectKind(nnkInfix)
    let
        (op, l, r) = (rel[0], rel[1], rel[2])
    if op.strVal == "==":
        quote:
            `self`.coerceEq(`l`, `r`)
    elif op.strVal == "<=":
        quote:
            `self`.coerceRelation(`l`, `r`)
    else:
        error fmt"permitted is {l} == {r} or {l} <= {r}.", op
        newEmptyNode()

const
    cvar = 1000
    undeciable = 10000
proc likelihoodimpl*(self: TypeEnv, t1, t2: Value): int =
    setTypeEnv(self)
    if t1 == t2:
        0
    elif t1.kind == t2.kind:
        case t1.kind
        of ValueKind.Var:
            undeciable
        of ValueKind.Pi:
            var ret = 0
            if t1.params.len != t2.params.len:
                undeciable
            else:
                for (t1, t2) in t1.params.zip(t2.params):
                    ret = self.likelihoodimpl(t2, t1)
                ret += self.likelihoodimpl(t1.rety, t2.rety)
                ret
        else:
            echo t1
            echo t2
            echo t1.symbol
            echo t2.symbol
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
proc likelihood*(self: TypeEnv, t1, t2: Value): seq[(Value, int)] =
    assert t1.kind == ValueKind.Intersection
    assert t2.kind != ValueKind.Intersection
    t1.types.mapIt((it, self.likelihoodimpl(it, t2)))

proc resolveRelation(self: TypeEnv, t1, t2: Value) =
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
            for (t1, t2) in t1.params.zip(t2.params):
                self.resolveRelation(t2, t1)
            self.resolveRelation(t1.rety, t2.rety)
        of ValueKind.Singleton:
            self.resolveRelation(t1.base, t2.base)
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
                echo t1
                echo t2
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

proc resolveRelationsPartially*(self: TypeEnv) =
    var tmp: seq[Constraint]
    while self.constraints.len > 0:
        self.constraints.reverse
        while self.constraints.len > 0:
            let
                (t1, t2) = self.constraints.pop
            self.resolveRelation(t1, t2)
        var
            ord = newOrder[Value]()
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
        self.constraints = self.tvconstraints & self.interconstraints
        self.tvconstraints = @[]
        self.interconstraints = @[]
        if self.constraints.len == 0:
            break
        if self.constraints.len == tmp.len and self.constraints.zip(tmp).allIt(it[0] == it[1]):
            break
        tmp = self.constraints

proc resolveRelations*(self: TypeEnv) =
    var tmp: seq[Constraint]
    while self.constraints.len > 0:
        self.constraints.reverse
        while self.constraints.len > 0:
            let
                (t1, t2) = self.constraints.pop
            self.resolveRelation(t1, t2)
        var
            ord = newOrder[Value]()
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
    self.tvs = self.tvs.filter(it => it.kind == ValueKind.Var).map(it => self.simplify(it))
    # for tv in self.tvs:
    #     if tv.tv.lb.compilable:
    #         self.bindtv(tv, tv.tv.lb)
    #     else:
    #         self.bindtv(tv, tv.tv.ub)
    # self.tvs.clear
