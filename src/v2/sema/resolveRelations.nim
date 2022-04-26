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

proc bindtv*(self: TypeEnv, v: Value) =
    assert v.kind == ValueKind.Var
    let (ins, outs) = self.order.clear(v)
    v[] = v.tv.ub[]
    for e in ins:
        self.order.add (e, v)
    for e in outs:
        self.order.add (v, e)
proc bindtv*(self: TypeEnv, v1: Value, v2: Value) =
    assert v1.kind == ValueKind.Var
    assert v2.kind != ValueKind.Var
    let (ins, outs) = self.order.clear(v1)
    v1[] = v2[]
    for e in ins.filter(it => (it != v2)):
        self.order.add (e, v2)
    for e in outs.filter(it => (it != v2)):
        self.order.add (v2, e)
        
proc collapse*(env: TypeEnv, scc: seq[Value]): Value =
    # if c has only one sort of concrete type (which is not type value), bind other type value into its type.
    # if c has only type values, generate new type value and bind other type values into Link type which is linked to the new one.
    # ohterwise, idk.
    if scc.len == 1: return scc[0]

    let concretes = scc.filterIt(it.kind != ValueKind.Var)

    var collapsed: Value

    if concretes.len >= 2:
        env.errs.add TypeError.CircularSubtype(concretes[0], concretes[1])
        collapsed = concretes[0]
        for n in scc.filterIt(it != collapsed):
            if n.kind == ValueKind.Var:
                env.bindtv(n, collapsed)
            else:
                let (ins, outs) = env.order.clear(n)
                for e in ins.filter(it => (it != collapsed)):
                    env.order.add (e, collapsed)
                for e in outs.filter(it => (it != collapsed)):
                    env.order.add (collapsed, e)
        return collapsed


    collapsed = if concretes.len == 1: concretes[0] else: Value.Var(env)
    for v in scc.filterIt(it.kind == ValueKind.Var):
        env.bindtv(v, collapsed)

    collapsed

proc resolve*(env: TypeEnv, v1: Value, v2: Value) =
    # i can assume v1 is not type value
    # if both of v1 and v2 are not type value, check v1 <= v2.
    # if v2 is type type value, update v2's upper bound by glb(v1, v2.tv.ub)
    discard
proc resolve*(env: TypeEnv, v: Value) =
    # if v is type value, bind it into its upper bound
    # if v is of intersection type, simplify it.
    discard
proc resolve*(self: TypeEnv) =
    var sorted: seq[Value] = @[]
    for scc in self.order.SCC:
        sorted.add self.collapse(scc)
    
    



proc `<=`(t1, t2: Value): bool {.error.}
template setTypeEnv(self: TypeEnv): untyped =
    proc `<=`(t1, t2: Value): bool =
        self.`<=`(t1, t2)

proc applicable(conv: (Value, Value), p: (Value, Value)): bool =
    false
proc `<=`(self: TypeEnv, t1, t2: Value): bool =
    setTypeEnv(self)
    if t1.kind == t2.kind:
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
        of ValueKind.Sum:
            false
        of ValueKind.Trait:
            false
        of ValueKind.Singleton:
            false
        of ValueKind.Distinct:
            false
        of ValueKind.Intersection:
            false
        of ValueKind.Union:
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
            t1.to <= t2.to
    else:
        if t2.kind == ValueKind.Unit:
            true
        elif t1.kind == ValueKind.Unit:
            false
        elif t1.kind == ValueKind.Bottom:
            true
        elif t2.kind == ValueKind.Bottom:
            false
        elif t1.kind == ValueKind.Var:
            (t1, t2) in self.order
        elif t2.kind == ValueKind.Var:
            (t1, t2) in self.order
        else:
            toSeq(self.scope.converters.keys).anyIt(it.applicable((t1, t2)))


proc simplify(self: TypeEnv, t: Value): Value =
    setTypeEnv(self)
    assert t.kind == ValueKind.Intersection
    let types = t.types
    var dels = initHashSet[Value]()
    for t1 in types:
        for t2 in types:
            if t1 <= t2:
                dels.incl t2
                break
    var res = types - dels
    assert res.len != 0
    if res.len == 1:
        res.pop
    else:
        Value.Intersection(res)
proc Intersection*(_: typedesc[Value], t1, t2: Value): Value =
    var
        res = initHashSet[Value]()
    if t1.kind == ValueKind.Intersection:
        res.incl t1.types
    else:
        res.incl t1
    if t2.kind == ValueKind.Intersection:
        res.incl t2.types
    else:
        res.incl t2
proc glb(self: TypeEnv, t1, t2: Value): Value =
    self.simplify(Value.Intersection(t1, t2))
