import sequtils
import options
import sets
import tables
import sugar

import coerce

import ../il
import ../typeenv
import ../orders
import ../errors
import ../utils
import ../dots


proc `<=`*(t1, t2: Value): bool {.error.}
proc `<=@`*(t1, t2: Value): Option[seq[Constraint]] {.error.}
proc `<=?`*(t1, t2: Value): Option[seq[Constraint]] {.error.}
proc `<=`*(self: TypeEnv, t1, t2: Value): bool
proc `<=@`*(self: TypeEnv, t1, t2: Value): bool
proc `<=?`*(env: TypeEnv, t1, t2: Value): Option[seq[Constraint]]
template setTypeEnv*(self: TypeEnv): untyped =
    proc `<=`(t1, t2: Value): bool =
        self.`<=`(t1, t2)
    proc `<=@`(t1, t2: Value): bool =
        self.`<=@`(t1, t2)
    proc `<=?`(t1, t2: Value): Option[seq[Constraint]] =
        self.`<=?`(t1, t2)

proc `<=@`*(self: TypeEnv, t1, t2: Value): bool =
    ## premitive <=
    setTypeEnv(self)
    if t1 == t2:
        true
    elif t1.kind == t2.kind:
        case t1.kind
        of ValueKind.Literal:
            t1.litval == t2.litval
        of ValueKind.Bottom, ValueKind.Unit, ValueKind.Bool, ValueKind.Char, ValueKind.String:
            true
        of ValueKind.Integer, ValueKind.Float:
            t1.bits <= t2.bits
        of ValueKind.Pair:
            t1.first <= t2.first and t1.second <= t2.second
        of ValueKind.Array:
            t1.base <= t2.base
        of ValueKind.ArrayV:
            t1.vals.zip(t2.vals).allIt(it[0] <= it[1])
        of ValueKind.Record:
            var res = true
            for key in t2.members.keys:
                if key in t1.members:
                    if t1.members[key] <= t2.members[key]:
                        discard
                    else:
                        res = false
                        break
                else:
                    res = false
                    break
            res
        of ValueKind.Ptr:
            t1.pointee <= t2.pointee
        of ValueKind.Pi:
            if t1.implicit.len == 0 and t2.implicit.len == 0:
                t1.rety <= t2.rety and
                t1.params.zip(t2.params).allIt(it[1] <= it[0])
            else:
                # TODO:
                false
        of ValueKind.Family:
            false
        of ValueKind.Sum:
            false
        of ValueKind.Trait:
            false
        of ValueKind.Singleton:
            false
        of ValueKind.Distinct:
            false
        of ValueKind.Intersection:
            t1.types.anyIt(self.`<=`(it, t2))
        of ValueKind.Union:
            t1.types.allIt(self.`<=`(it, t2))
        of ValueKind.Select:
            false
        of ValueKind.Cons:
            false
        of ValueKind.Lambda:
            false
        of ValueKind.Var:
            (t1, t2) in self.order
        of ValueKind.Gen:
            false
        of ValueKind.Link:
            t1.to <=@ t2.to
    else:
        if t2.kind == ValueKind.Unit:
            true
        elif t1.kind == ValueKind.Unit:
            false
        elif t1.kind == ValueKind.Bottom:
            true
        elif t2.kind == ValueKind.Bottom:
            false
        elif t2.kind == ValueKind.Distinct:
            t1 <= t2.base
        elif t1.kind == ValueKind.Intersection:
            t1.types.anyIt(self.`<=`(it, t2))
        elif t2.kind == ValueKind.Intersection:
            t2.types.allIt(self.`<=`(t1, it))
        elif t1.kind == ValueKind.Union:
            t1.types.allIt(self.`<=`(it, t2))
        elif t2.kind == ValueKind.Union:
            t2.types.anyIt(self.`<=`(t1, it))
        elif t1.kind == ValueKind.Var:
            (t1, t2) in self.order
        elif t2.kind == ValueKind.Var:
            (t1, t2) in self.order
        else:
            false

proc path*(self: TypeEnv, t1, t2: Value): seq[seq[(Value, Value)]] =
    if t1.kind == ValueKind.Link:
        return self.path(t1.to, t2)
    if t2.kind == ValueKind.Link:
        return self.path(t1, t2.to)
    setTypeEnv(self)
    if t1 <=@ t2:
        return @[@[(t1, t2)]]
    var
        converters1: seq[(Value, Value)]
        converters2: seq[(Value, Value)]
    for it in self.scope.converters.keys:
        if it[1] == t2:
            converters1.add it
        elif self.`<=@`(it[1], t2):
            converters2.add it
    for (s, _) in converters1:
        if t1 == s:
            result.add @[(s, t2)]
        else:
            result.add self.path(t1, s).mapIt(it & (s, t2))
    for (s, d) in converters2:
        if t1 == s:
            result.add @[(s, d), (d, t2)]
        else:
            result.add self.path(t1, s).mapIt(it & @[(s, d), (d, t2)])

proc applicable(conv: (Value, Value), p: (Value, Value)): bool =
    false

proc `<=`*(self: TypeEnv, t1, t2: Value): bool =
    setTypeEnv(self)
    if t1 <=@ t2:
        true
    else:
        let converters = toSeq(self.scope.converters.keys).filterIt(it[1] <=@ t2)
        converters.anyIt(t1 <= it[0])

proc `<=?`*(env: TypeEnv, t1, t2: Value): Option[seq[Constraint]] =
    setTypeEnv(env)
    if t1.kind == ValueKind.Link:
        env.`<=?`(t1.to, t2)
    elif t2.kind == ValueKind.Link:
        env.`<=?`(t1, t2.to)
    elif t1 <= t2:
        some newSeq[Constraint]()
    elif t1.kind == ValueKind.Select:
        if t1.types.anyIt((it <=? t2).isSome):
            some newSeq[Constraint]()
        else:
            none seq[Constraint]
    elif t2.kind == ValueKind.Select:
        if t2.types.anyIt((t1 <=? it).isSome):
            some newSeq[Constraint]()
        else:
            none seq[Constraint]
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
        # TODO: replace it with `applicable`
        if env.scope.typeOrder.path(t1, t2).isSome:
            some newSeq[Constraint]()
        else:
            none(seq[Constraint])

proc Union*(_: typedesc[Value], t1, t2: Value): Value =
    var
        res = initHashSet[Value]()
    if t1.kind == ValueKind.Union:
        res.incl t1.types
    else:
        res.incl t1
    if t2.kind == ValueKind.Union:
        res.incl t2.types
    else:
        res.incl t2
    Value.Union(res)
proc glb(self: TypeEnv, t1, t2: Value): Value =
    result = Value.Unit
    setTypeEnv(self)
    result = if t1.kind != ValueKind.Intersection:
        if t2.kind == ValueKind.Intersection:
            toSeq(t2.types).foldl(self.glb(a, b), result)
            # for t2 in t2.types:
                # result = self.glb(result, t2)
        else:
            if t1 <= t2:
                t1
            elif t2 <= t1:
                t2
            else:
                Value.Intersection(@[t1, t2])
    else:
        if t2.kind == ValueKind.Intersection:
            toSeq(t2.types).foldl(self.glb(a, b), result)
            # for t2 in t2.types:
            #     result = self.glb(result, t2)
        else:
            if toSeq(t1.types).anyIt(it <= t2):
                t1
            else:
                var res = t1.types
                for e in res:
                    if t2 <= e:
                        res.excl e
                res.incl t2
                Value.Intersection(res)
proc glb(self: TypeEnv, types: seq[Value]): Value =
    types.foldl(self.glb(a, b))
proc lub(self: TypeEnv, t1, t2: Value): Value =
    result = Value.Bottom
    setTypeEnv(self)
    assert t1.kind != ValueKind.Select or t2.kind != ValueKind.Select
    result = if t1.kind != ValueKind.Union:
        if t2.kind == ValueKind.Union:
            toSeq(t2.types).foldl(self.lub(a, b), result)
        else:
            if t1 <= t2:
                t2
            elif t2 <= t1:
                t1
            else:
                Value.Union(@[t1, t2])
    else:
        if t2.kind == ValueKind.Union:
            toSeq(t2.types).foldl(self.lub(a, b), result)
        else:
            if toSeq(t1.types).anyIt(t2 <= it):
                t1
            else:
                var res = t1.types
                for e in res:
                    if e <= t2:
                        res.excl e
                res.incl t2
                Value.Union(res)
proc lub(self: TypeEnv, types: seq[Value]): Value =
    types.foldl(self.lub(a, b))
proc simplify(self: TypeEnv, t: Value): Value =
    assert t.kind in {ValueKind.Intersection, ValueKind.Union}
    if t.kind == ValueKind.Intersection:
        self.glb(toSeq(t.types))
    else:
        self.lub(toSeq(t.types))

proc bindtv*(self: TypeEnv, v: Value) =
    assert v.kind == ValueKind.Var
    let (ins, outs) = self.order.clear(v)
    # TODO: add ident
    let symbol = if v.symbol.isSome: v.symbol else: v.tv.ub.symbol
    v[] = v.tv.ub[]
    v.symbol = symbol
    for e in ins:
        self.order.add (e, v)
    for e in outs:
        self.order.add (v, e)
proc bindtv*(self: TypeEnv, v1: Value, v2: Value) =
    assert v1.kind in {ValueKind.Var, ValueKind.Select}
    # assert v2.kind != ValueKind.Var
    let (ins, outs) = self.order.clear(v1)
    # TODO: add ident
    let symbol = if v1.symbol.isSome: v1.symbol else: v2.symbol
    if v1.kind == ValueKind.Select:
        if v2.kind == ValueKind.Select:
            v1[] = v2[]
        elif v2.kind in {ValueKind.Var, ValueKind.Link}:
            v1[] = Value.Link(v2)[]
        else:
            v1[] = v2[]
    elif v2.kind in {ValueKind.Var, ValueKind.Link}:
        v1[] = Value.Link(v2)[]
    else:
        v1[] = v2[]
    v1.symbol = symbol
    let target = if v2.kind == ValueKind.Link: v2.to else: v2
    for e in ins.filter(it => (it != target)):
        self.order.add (e, target)
    for e in outs.filter(it => (it != target)):
        self.order.add (target, e)

proc expand(self: TypeEnv, node: Value) =
    if node.kind == ValueKind.Var: return
    if self.order.outdegree(node) == 0: return

    for target in self.order.primal[node]:
        if node.kind == target.kind:
            case node.kind
            of ValueKind.Var:
                assert false
            of ValueKind.Literal..ValueKind.String:
                discard
            of ValueKind.Pi:
                assert node.params.len == target.params.len
                for param in node.params.zip(target.params):
                    self.order.add((param[1], param[0]))
                self.order.add((node.rety, target.rety))
                self.order.remove((node, target))
            else:
                # TODO:
                discard

proc collapse(self: TypeEnv, scc: seq[Value]): Value =
    # if c has only one sort of concrete type (which is not type value), bind other type value into its type.
    # if c has only type values, generate new type value and bind other type values into Link type which is linked to the new one.
    # ohterwise, idk.
    setTypeEnv(self)

    if scc.len == 1: return scc[0]

    let concretes = scc.filterIt(it.kind != ValueKind.Var)

    var collapsed: Value

    if concretes.len >= 1:

        if concretes.len >= 2 and (not concretes.allIt((it <=? concretes[0]).isSome and (concretes[0] <=? it).isSome)):
            self.errs.add TypeError.CircularSubtype(concretes[0], concretes[1])

        collapsed = concretes[0]

        for n in scc.filterIt(it != collapsed):
            if n.kind == ValueKind.Var:
                self.bindtv(n, collapsed)
            else:
                let (ins, outs) = self.order.clear(n)
                for e in ins.filter(it => (it != collapsed)):
                    self.order.add (e, collapsed)
                for e in outs.filter(it => (it != collapsed)):
                    self.order.add (collapsed, e)
    else:
        collapsed = Value.Var(self)
        for n in scc.filterIt(it != collapsed):
            self.bindtv(n, Value.Link(collapsed))

    collapsed

proc resolve(self: TypeEnv, v1: Value, v2: Value, primal: bool): bool =
    setTypeEnv(self)
    # i can assume v1 is not type value
    # if both of v1 and v2 are not type value, check v1 <= v2.
    # if v2 is type type value, update v2's upper bound by glb(v1, v2.tv.ub)

    let
        v =
            if v1.kind == ValueKind.Var:
                if primal: v1.tv.lb else: v1.tv.ub
            elif v1.kind == ValueKind.Select:
                if v1.types.len == 0:
                    if primal:
                        Value.Bottom
                    else:
                        Value.Unit
                else:
                    if primal:
                        Value.Intersection(v1.types)
                    else:
                        Value.Union(v1.types)
            else:
                v1

    result = false

    if v2.kind == ValueKind.Var:
        if primal:
            let lub = self.lub(v, v2.tv.lb)
            if v2.tv.lb != lub: result = true
            v2.tv.lb = lub
        else:
            let glb = self.glb(v, v2.tv.ub)
            if v2.tv.ub != glb: result = true
            v2.tv.ub = glb
    elif v2.kind == ValueKind.Select:
        if primal:
            var lub = initHashSet[Value]()
            for e in v2.types:
                if (v <=? e).isSome:
                    lub.incl e
            if v2.types != lub: result = true
            v2.types = lub
        else:
            var glb = initHashSet[Value]()
            for e in v2.types:
                if (e <=? v).isSome:
                    glb.incl e
            if v2.types != glb: result = true
            v2.types = glb

    # else:
    #     if primal and not self.`<=?`(v, v2).isNone:
    #         self.errs.add TypeError.Subtype(v, v2)
    #     elif not primal and not self.`<=?`(v2, v).isNone:
    #         self.errs.add TypeError.Subtype(v2, v)
proc coerce(self: TypeEnv, v: Value) =
    if v in self.order.primal:
        for e in self.order.primal[v]:
            coerce.coerce(self, v <= e)
    if v in self.order.dual:
        for e in self.order.dual[v]:
            coerce.coerce(self, e <= v)
proc resolve(self: TypeEnv, v: Value, primal: bool=true): bool =
    # if v is type value, bind it into its upper bound
    # if v is of intersection type, simplify it.
    let
        relation = if primal: self.order.primal else: self.order.dual

    result = false

    if v notin relation: return

    for target in relation[v]:
        if self.resolve(v, target, primal):
            result = true
    self.coerce(v)

proc greatest(self: TypeEnv, v: Value): Option[Value] =
    # TODO: nanikasuru
    setTypeEnv(self)
    assert v.kind == ValueKind.Select
    if v.types.len == 1:
        return some toSeq(v.types)[0]

    let
        tmp = v.types.filter(it => not it.symbol.get.typ.isPolymorphic)
        types = tmp.filter(it1 => tmp.all(it2 => it2 <= it1))
    case types.len
    of 0:
        none(Value)
    of 1:
        some toSeq(types)[0]
    else:
        none(Value)
proc update(self: TypeEnv): bool {.discardable.} =
    for t1, t2s in self.order.primal.pairs:
        for t2 in t2s:
            self.constraints.add (t1, t2)
    self.order.primal.clear()
    self.order.dual.clear()
    while self.constraints.len > 0:
        self.order.add self.constraints.pop
    self.tvs = self.tvs.filter(it => it.kind == ValueKind.Var)
    self.selects = self.selects.filter(it => it.kind == ValueKind.Select)
    not (self.tvs.len == 0 and self.selects.len == 0)
proc resolve*(self: TypeEnv) =
    setTypeEnv(self)

    when not defined(release):
        var
            dot = newDot[Value]()
    var
        isChanged = true
        sorted: seq[Value]
    self.update
    when not defined(release):
        dot.add self.order
    template cont: untyped =
        self.update
        when not defined(release):
            dot.add self.order
        if isChanged:
            continue
    while self.update and isChanged:
        isChanged = false
        sorted = self.order.SCC.mapIt(self.collapse(it))

        cont

        for n in sorted:
            if self.resolve(n, primal=true):
                isChanged = true
        for n in sorted.reversed:
            if self.resolve(n, primal=false):
                isChanged = true
        for n in sorted:
            if self.resolve(n, primal=true):
                isChanged = true
        for n in sorted.reversed:
            if self.resolve(n, primal=false):
                isChanged = true
        for e in self.selects:
            if e.types.len == 1:
                self.bindtv(e, e.types.pop)
                self.coerce(e)
                isChanged = true
        cont

        for e in self.tvs:
            assert e.kind == ValueKind.Var
            if e.tv.ub == e.tv.lb and e.tv.ub.compilable:
                self.bindtv(e, e.tv.ub)
                isChanged = true
        cont

        # for e in sorted.filterIt(it.kind == ValueKind.Select):
        for e in self.selects:
            let g = self.greatest(e)
            if g.isSome and g.get.kind == ValueKind.Pi:
                self.bindtv(e, g.get)
                self.coerce(e)
                isChanged = true
        cont

        # for n in sorted.reversed:
        for e in self.tvs:
            if e.tv.lb <= e.tv.ub and e.tv.lb.compilable:
                self.bindtv(e, e.tv.lb)
                self.coerce(e)
                isChanged = true
        cont
        for e in self.tvs:
            if e.tv.lb <= e.tv.ub and e.tv.ub.compilable:
                self.bindtv(e, e.tv.ub)
                self.coerce(e)
                isChanged = true
        cont
    for e in self.tvs:
        if e.tv.lb <= e.tv.ub:
            self.bindtv(e, e.tv.lb) # TODO: ub?
        else:
            self.bindtv(e, e.tv.lb)
    # when not defined(release):
    #     dot.save("./dots")

when isMainModule:
    import eval
    import scopes
    import ../parsers

    let
        f = open("test/unit.rgn")
        s = f.readAll
        program = Program(Source.from(s)).get
        mainScope = program.setScope
        env = newTypeEnv(mainScope)
    for s in program.stmts:
        discard s.infer(env)
    env.resolve

    debug env.tvs
    debug env.errs
    debug program
