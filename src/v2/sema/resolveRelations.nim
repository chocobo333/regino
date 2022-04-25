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

type Graph = Table[Value, seq[Value]]


proc resolve*(env: TypeEnv, v: Value) =
    discard

proc resolve*(env: TypeEnv) =
    var graph: Graph
    for (target, source) in env.constraints:
        if source in graph:
            graph[source].add target
        else:
            graph[source] = @[target]


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
