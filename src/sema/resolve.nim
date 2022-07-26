import sequtils
import options
import sets
import tables
import sugar

import coerce

import ir
import typeenvs
import ../orders
import ../dots
import ../utils


proc `<=`*(t1, t2: Type): bool {.error.}
proc `<=@`*(t1, t2: Type): Option[seq[Constraint]] {.error.}
proc `<=?`*(t1, t2: Type): Option[seq[Constraint]] {.error.}
proc `<=`*(self: TypeEnv, t1, t2: Type): bool
proc `<=@`*(self: TypeEnv, t1, t2: Type): bool
proc `<=?`*(env: TypeEnv, t1, t2: Type): Option[seq[Constraint]]
template setTypeEnv*(self: TypeEnv): untyped =
    proc `<=`(t1, t2: Type): bool =
        self.`<=`(t1, t2)
    proc `<=@`(t1, t2: Type): bool =
        self.`<=@`(t1, t2)
    proc `<=?`(t1, t2: Type): Option[seq[Constraint]] =
        self.`<=?`(t1, t2)

proc `<=@`*(self: TypeEnv, t1, t2: Type): bool =
    ## premitive <=
    setTypeEnv(self)
    if t1 == t2:
        true
    elif t1.kind == t2.kind:
        case t1.kind
        of TypeKind.Univ:
            t1.level <= t2.level
        of TypeKind.Value:
            t1.val == t2.val
        of TypeKind.Bottom, TypeKind.Unit, TypeKind.Bool, TypeKind.Char, TypeKind.CString:
            true
        of TypeKind.Integer, TypeKind.Float:
            t1.nbits <= t2.nbits
        of TypeKind.Pair:
            t1.first <= t2.first and t1.second <= t2.second
        of TypeKind.Array:
            t1.base <= t2.base
        of TypeKind.Record, TypeKind.Object:
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
        of TypeKind.Ptr:
            t1.pointee <= t2.pointee and t2.pointee <= t1.pointee
        of TypeKind.Arrow:
            t1.rety <= t2.rety and
            t1.params.zip(t2.params).allIt(it[1] <= it[0])
        # of TypeKind.Sum:
        #     false
        of TypeKind.Trait:
            false
        of TypeKind.Singleton:
            false
        of TypeKind.Distinct:
            false
        of TypeKind.Intersection:
            t1.types.anyIt(self.`<=`(it, t2))
        of TypeKind.Union:
            t1.types.allIt(self.`<=`(it, t2))
        of TypeKind.Cons:
            false
        # of TypeKind.Lambda:
        #     false
        of TypeKind.Recursive:
            false
        of TypeKind.Var:
            (t1, t2) in self.order
        of TypeKind.Select:
            # TODO:
            false
        of TypeKind.RecursiveVar:
            t1.id == t2.id
        of TypeKind.Gen:
            false
        of TypeKind.Link:
            t1.to <=@ t2.to
    else:
        if t2.kind == TypeKind.Unit:
            true
        elif t1.kind == TypeKind.Unit:
            false
        elif t1.kind == TypeKind.Bottom:
            true
        elif t2.kind == TypeKind.Bottom:
            false
        elif t2.kind == TypeKind.Distinct:
            t1 <= t2.base
        elif t1.kind == TypeKind.Intersection:
            t1.types.anyIt(self.`<=`(it, t2))
        elif t2.kind == TypeKind.Intersection:
            t2.types.allIt(self.`<=`(t1, it))
        elif t1.kind == TypeKind.Union:
            t1.types.allIt(self.`<=`(it, t2))
        elif t2.kind == TypeKind.Union:
            t2.types.anyIt(self.`<=`(t1, it))
        elif t1.kind == TypeKind.Var:
            (t1, t2) in self.order
        elif t2.kind == TypeKind.Var:
            (t1, t2) in self.order
        else:
            false

proc path*(self: TypeEnv, t1, t2: Type): seq[seq[(Type, Type)]] =
    if t1.kind == TypeKind.Link:
        return self.path(t1.to, t2)
    if t2.kind == TypeKind.Link:
        return self.path(t1, t2.to)
    setTypeEnv(self)
    if t1 <=@ t2:
        return @[@[(t1, t2)]]
    var
        converters1: seq[(Type, Type)]
        converters2: seq[(Type, Type)]
    for it in self.scope.getConverters().keys:
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

proc applicable(conv: (Type, Type), p: (Type, Type)): bool =
    false

proc `<=`*(self: TypeEnv, t1, t2: Type): bool =
    setTypeEnv(self)
    if t1 <=@ t2:
        true
    else:
        let converters = toSeq(self.scope.getConverters().keys).filterIt(it[1] <=@ t2)
        converters.anyIt(t1 <= it[0])

proc `<=?`*(env: TypeEnv, t1, t2: Type): Option[seq[Constraint]] =
    setTypeEnv(env)
    if t1.kind == TypeKind.Link:
        env.`<=?`(t1.to, t2)
    elif t2.kind == TypeKind.Link:
        env.`<=?`(t1, t2.to)
    elif t1 <= t2:
        some newSeq[Constraint]()
    elif t1.kind == TypeKind.Select:
        if t1.types.anyIt((it <=? t2).isSome):
            some newSeq[Constraint]()
        else:
            none seq[Constraint]
    elif t2.kind == TypeKind.Select:
        if t2.types.anyIt((t1 <=? it).isSome):
            some newSeq[Constraint]()
        else:
            none seq[Constraint]
    elif t1.kind == t2.kind:
        case t1.kind
        of TypeKind.Bottom, TypeKind.Unit, TypeKind.Integer, TypeKind.Float, TypeKind.Char, TypeKind.CString:
            some newSeq[Constraint]()
        of TypeKind.Pair:
            let
                first = env.`<=?`(t1.first, t2.first)
                second = env.`<=?`(t1.second, t2.second)
            if first.isSome and second.isSome:
                some first.get & second.get
            else:
                none(seq[Constraint])
        of TypeKind.Record:
            let
                ret = toSeq(t2.members.keys).mapIt(`<=?`(env, t1.members.getOrDefault(it, Type.Unit), t2.members[it]))
            if ret.allIt(it.isSome):
                some ret.mapIt(it.get).flatten
            else:
                none(seq[Constraint])
        of TypeKind.Arrow:
            let
                params = t1.params.zip(t2.params).mapIt(env.`<=?`(it[1], it[0]))
                rety = env.`<=?`(t1.rety, t2.rety)
            if params.allIt(it.isSome) and rety.isSome:
                some params.mapIt(it.get).flatten & rety.get
            else:
                none(seq[Constraint])
        of TypeKind.Var:
            if t1.lb <= t2.ub:
                some @[(t1, t2)]
            else:
                none(seq[Constraint])
        else:
            # env.raise("not implemented")
            assert false
            none(seq[Constraint])
    elif t2.kind == TypeKind.Var:
        # if t2.ub != t1 and t2.ub <= t1:
        #     none(seq[Constraint])
        # else:
        #     some @[(t1, t2)]
        if t1 <= t2.ub:
            some @[(t1, t2)]
        else:
            none(seq[Constraint])
    elif t1.kind == TypeKind.Var:
        # if t2 != t1.lb and t2 <= t1.lb:
        #     none(seq[Constraint])
        # else:
        #     some @[(t1, t2)]
        if t1.lb <= t2:
            some @[(t1, t2)]
        else:
            none(seq[Constraint])
    else:
        # # TODO: replace it with `applicable`
        # if env.scope.typeOrder.path(t1, t2).isSome:
        #     some newSeq[Constraint]()
        # else:
            none(seq[Constraint])

proc Union*(_: typedesc[Type], t1, t2: Type): Type =
    var
        res = initHashSet[Type]()
    if t1.kind == TypeKind.Union:
        res.incl t1.types
    else:
        res.incl t1
    if t2.kind == TypeKind.Union:
        res.incl t2.types
    else:
        res.incl t2
    Type.Union(res)
proc glb(self: TypeEnv, t1, t2: Type): Type =
    result = Type.Unit
    setTypeEnv(self)
    result = if t1.kind != TypeKind.Intersection:
        if t2.kind == TypeKind.Intersection:
            toSeq(t2.types).foldl(self.glb(a, b), result)
            # for t2 in t2.types:
                # result = self.glb(result, t2)
        else:
            if t1 <= t2:
                t1
            elif t2 <= t1:
                t2
            else:
                Type.Intersection(@[t1, t2].toHashSet)
    else:
        if t2.kind == TypeKind.Intersection:
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
                Type.Intersection(res)
proc glb(self: TypeEnv, types: seq[Type]): Type =
    types.foldl(self.glb(a, b))
proc lub(self: TypeEnv, t1, t2: Type): Type =
    result = Type.Bottom
    setTypeEnv(self)
    assert t1.kind != TypeKind.Select or t2.kind != TypeKind.Select
    result = if t1.kind != TypeKind.Union:
        if t2.kind == TypeKind.Union:
            toSeq(t2.types).foldl(self.lub(a, b), result)
        else:
            if t1 <= t2:
                t2
            elif t2 <= t1:
                t1
            else:
                Type.Union(@[t1, t2].toHashSet)
    else:
        if t2.kind == TypeKind.Union:
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
                Type.Union(res)
proc lub(self: TypeEnv, types: seq[Type]): Type =
    types.foldl(self.lub(a, b))
proc simplify(self: TypeEnv, t: Type): Type =
    assert t.kind in {TypeKind.Intersection, TypeKind.Union}
    if t.kind == TypeKind.Intersection:
        self.glb(toSeq(t.types))
    else:
        self.lub(toSeq(t.types))

proc bindtv*(self: TypeEnv, v: Type) =
    assert v.kind == TypeKind.Var
    let (ins, outs) = self.order.clear(v)
    # TODO: add ident
    let symbol = if v.symbol.isSome: v.symbol else: v.ub.symbol
    v[] = v.ub[]
    v.symbol = symbol
    for e in ins:
        self.order.add (e, v)
    for e in outs:
        self.order.add (v, e)
proc bindtv*(self: TypeEnv, v1: Type, v2: Type) =
    assert v1.kind in {TypeKind.Var, TypeKind.Select}
    # assert v2.kind != TypeKind.Var
    let (ins, outs) = self.order.clear(v1)
    # TODO: add ident
    let symbol = if v1.symbol.isSome: v1.symbol else: v2.symbol
    if v1.kind == TypeKind.Select:
        if v2.kind == TypeKind.Select:
            v1[] = v2[]
        elif v2.kind in {TypeKind.Var, TypeKind.Link}:
            v1[] = Type.Link(v2)[]
        else:
            v1[] = v2[]
    elif v2.kind in {TypeKind.Var, TypeKind.Link}:
        v1[] = Type.Link(v2)[]
    else:
        v1[] = v2[]
    v1.symbol = symbol
    let target = if v2.kind == TypeKind.Link: v2.to else: v2
    for e in ins.filter(it => (it != target)):
        self.order.add (e, target)
    for e in outs.filter(it => (it != target)):
        self.order.add (target, e)

proc expand(self: TypeEnv, node: Type) =
    if node.kind == TypeKind.Var: return
    if self.order.outdegree(node) == 0: return

    for target in self.order.primal[node]:
        if node.kind == target.kind:
            case node.kind
            of TypeKind.Var:
                assert false
            of TypeKind.Value..TypeKind.CString:
                discard
            of TypeKind.Arrow:
                assert node.params.len == target.params.len
                for param in node.params.zip(target.params):
                    self.order.add((param[1], param[0]))
                self.order.add((node.rety, target.rety))
                self.order.remove((node, target))
            else:
                # TODO:
                discard

proc collapse(self: TypeEnv, scc: seq[Type]): Type =
    # if c has only one sort of concrete type (which is not type value), bind other type value into its type.
    # if c has only type values, generate new type value and bind other type values into Link type which is linked to the new one.
    # ohterwise, idk.
    setTypeEnv(self)

    if scc.len == 1: return scc[0]

    let concretes = scc.filterIt(it.kind != TypeKind.Var)

    var collapsed: Type

    if concretes.len >= 1:

        if concretes.len >= 2 and (not concretes.allIt((it <=? concretes[0]).isSome and (concretes[0] <=? it).isSome)):
            # self.errs.add TypeError.CircularSubtype(concretes[0], concretes[1])
            assert false

        collapsed = concretes[0]

        for n in scc.filterIt(it != collapsed):
            if n.kind == TypeKind.Var:
                self.bindtv(n, collapsed)
            else:
                let (ins, outs) = self.order.clear(n)
                for e in ins.filter(it => (it != collapsed)):
                    self.order.add (e, collapsed)
                for e in outs.filter(it => (it != collapsed)):
                    self.order.add (collapsed, e)
    else:
        collapsed = Type.Var(self)
        for n in scc.filterIt(it != collapsed):
            self.bindtv(n, Type.Link(collapsed))

    collapsed

proc resolve(self: TypeEnv, v1: Type, v2: Type, primal: bool): bool =
    setTypeEnv(self)
    # i can assume v1 is not type value
    # if both of v1 and v2 are not type value, check v1 <= v2.
    # if v2 is type type value, update v2's upper bound by glb(v1, v2.tv.ub)

    let
        v =
            if v1.kind == TypeKind.Var:
                if primal: v1.lb else: v1.ub
            elif v1.kind == TypeKind.Select:
                if v1.types.len == 0:
                    if primal:
                        Type.Bottom
                    else:
                        Type.Unit
                else:
                    if primal:
                        Type.Intersection(v1.types)
                    else:
                        Type.Union(v1.types)
            else:
                v1

    result = false

    if v2.kind == TypeKind.Var:
        if primal:
            let lub = self.lub(v, v2.lb)
            if v2.lb != lub: result = true
            v2.lb = lub
        else:
            let glb = self.glb(v, v2.ub)
            if v2.ub != glb: result = true
            v2.ub = glb
    elif v2.kind == TypeKind.Select:
        if primal:
            var lub = initHashSet[Type]()
            for e in v2.types:
                if (v <=? e).isSome:
                    lub.incl e
            if v2.types != lub: result = true
            v2.types = lub
        else:
            var glb = initHashSet[Type]()
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
proc coerce(self: TypeEnv, v: Type) =
    if v in self.order.primal:
        for e in self.order.primal[v]:
            coerce.coerce(self, v <= e)
    if v in self.order.dual:
        for e in self.order.dual[v]:
            coerce.coerce(self, e <= v)
proc resolve(self: TypeEnv, v: Type, primal: bool=true): bool =
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

proc greatest(self: TypeEnv, v: Type): Option[Type] =
    # TODO: nanikasuru
    setTypeEnv(self)
    assert v.kind == TypeKind.Select
    if v.types.len == 1:
        return some toSeq(v.types)[0]

    let
        tmp = v.types
        types = tmp.filter(it1 => tmp.all(it2 => it2 <= it1))
    case types.len
    of 0:
        none(Type)
    of 1:
        some toSeq(types)[0]
    else:
        none(Type)
proc update(self: TypeEnv): bool {.discardable.} =
    for t1, t2s in self.order.primal.pairs:
        for t2 in t2s:
            self.constraints.add (t1, t2)
    self.order.primal.clear()
    self.order.dual.clear()
    while self.constraints.len > 0:
        self.order.add self.constraints.pop
    self.tvs = self.tvs.filter(it => it.kind == TypeKind.Var)
    self.selects = self.selects.filter(it => it.kind == TypeKind.Select)
    not (self.tvs.len == 0 and self.selects.len == 0)
proc resolveEq*(self: TypeEnv) =
    self.update
    discard self.order.SCC.mapIt(self.collapse(it))
proc resolve*(self: TypeEnv) =
    setTypeEnv(self)

    when not defined(release):
        var
            dot = newDot[Type]()
    var
        isChanged = true
        sorted: seq[Type]
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
        cont
        for n in sorted.reversed:
            if self.resolve(n, primal=false):
                isChanged = true
        cont
        for n in sorted:
            if self.resolve(n, primal=true):
                isChanged = true
        cont
        for n in sorted.reversed:
            if self.resolve(n, primal=false):
                isChanged = true
        cont
        for e in self.selects:
            if e.choices.len == 1:
                self.bindtv(e, e.choices.pop)
                self.coerce(e)
                isChanged = true
        cont

        for e in self.tvs:
            assert e.kind == TypeKind.Var
            if e.ub == e.lb and e.ub.compilable:
                self.bindtv(e, e.ub)
                isChanged = true
        cont

        # for e in sorted.filterIt(it.kind == TypeKind.Select):
        for e in self.selects:
            let g = self.greatest(e)
            if g.isSome and g.get.kind == TypeKind.Arrow:
                self.bindtv(e, g.get)
                self.coerce(e)
                isChanged = true
        cont

        # for n in sorted.reversed:
        for e in self.tvs:
            if e.lb <= e.ub and e.lb.compilable:
                self.bindtv(e, e.lb)
                self.coerce(e)
                isChanged = true
        cont
        for e in self.tvs:
            if e.lb <= e.ub and e.ub.compilable:
                self.bindtv(e, e.ub)
                self.coerce(e)
                isChanged = true
        cont
    for e in self.tvs:
        if e.lb <= e.ub:
            self.bindtv(e, e.lb) # TODO: ub?
        else:
            self.bindtv(e, e.lb)
    # when not defined(release):
    #     dot.save("./dots")

when isMainModule:
    import decls
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
