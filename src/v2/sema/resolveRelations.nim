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

proc reversed*(graph: ref Graph): Graph =
    var ret: Graph
    for source, targets in graph:
        for target in targets:
            if target in ret:
                ret[target].add source
            else:
                ret[target] = @[source]
    ret

proc SCC*(graph: ref Graph): seq[seq[Value]] =
    var
        reversedGraph = graph.reversed
        visited: HashSet[Value]
    @[]

proc resolveRelations*(env: TypeEnv) =
    var graph: Graph
    for (target, source) in env.constraints:
        if source in graph:
            graph[source].add target
        else:
            graph[source] = @[target]


proc `<=`(t1, t2: Value): bool {.error.}
proc `<=`(self: TypeEnv, t1, t2: Value): bool =
    false
template setTypeEnv(self: TypeEnv): untyped =
    proc `<=`(t1, t2: Value): bool =
        self.`<=`(t1, t2)

proc simplify(self: TypeEnv, t: Value): Value =
    setTypeEnv(self)
    assert t.kind == ValueKind.Intersection
    let types = t.types
    var dels = initHashSet[Value]()
    for t1 in types:
        for t2 in types:
            if t1 <= t2:
                dels.incl t1
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
