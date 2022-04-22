
import relations
import strformat
import sequtils
import strutils
import tables
import options
import sugar
import sets

import utils


type
    Order*[T] = object
        primal*: Relation[T, T] # for <
        dual*: Relation[T, T]   # for >

proc newOrder*[T](): Order[T] =
    Order[T](
        primal: newRelation[T, T](),
        dual: newRelation[T, T]()
    )

proc contains*[T](self: Order[T], val: (T, T)): bool =
    self.primal.contains val
proc add*[T](self: var Order[T], val: (T, T)) =
    self.primal[val[0]] = val[1]
    self.dual[val[1]] = val[0]
proc `$`*[T](self: Order[T]): string =
    if self.primal.len == 0:
        result = "{}"
    else:
        result = "{"
        for key, val in pairs(self.primal):
            for val in val.items:
                result.add fmt"{key} < {val}, "
        result = result[0..^3]
        result.add("}")
proc sort*[T](self: Order[T]): seq[T] =
    var indegree = initTable[T, int]()
    for key in self.primal.keys:
        indegree[key] = 0
    for key in self.dual.keys:
        indegree[key] = self.dual[key].len

    while indegree.len > 0:
        for e in indegree.filter(it => it == 0).keys:
            result.add e
            if e in self.primal:
                for ee in self.primal[e]:
                    indegree[ee] = indegree[ee] - 1
            indegree.del(e)



proc path*[T](self: Order[T], t1, t2: T): Option[seq[(T, T)]] =
    if (t1, t2) in self:
        some @[(t1, t2)]
    else:
        if t1 in self.primal:
            var
                l = int.high
                shortest: seq[(T, T)]
                t: T
            for e in self.primal[t1]:
                let p = self.path(e, t2)
                if p.isSome:
                    if p.get.len < l:
                        l = p.get.len
                        shortest = p.get
                        t = e
            if shortest.len == 0:
                none(seq[(T, T)])
            else:
                some((t1, t) & shortest)
        else:
            none(seq[(T, T)])

proc nodes*[T](self: Order[T]): HashSet[T] =
    let
        a = toSeq(self.primal.keys).toHashSet
        b = toSeq(self.dual.keys).toHashSet
    result = initHashSet[T]()
    for e in a.items:
        result.incl e
    for e in b.items:
        result.incl e
proc dot*[T](self: Order[T]): string =
    for n in self.nodes.items:
        result.add &"{n}\n"
    for key in self.primal.keys:
        for v in self.primal[key].items:
            result.add &"{key} -> {v}\n"
    result = result[0..^2]
    result = result.indent(2)
    result = &"digraph order {{\n{result}\n}}"
