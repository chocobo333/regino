
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
proc indegree*[T](self: Order[T], key: T): int =
    if key in self.dual:
        self.dual[key].len
    else:
        0
proc outdegree*[T](self: Order[T], key: T): int =
    if key in self.primal:
        self.primal[key].len
    else:
        0
proc add*[T](self: var Order[T], val: (T, T)) =
    self.primal[val[0]] = val[1]
    self.dual[val[1]] = val[0]
proc remove*[T](self: var Order[T], val: (T, T)) =
    self.primal.remove(val)
    self.dual.remove((val[1], val[0]))
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
proc clear*[T](self: var Order[T], key: T): (HashSet[T], HashSet[T]) {.discardable.} =
    let a =
        if key in self.dual:
            self.dual[key]
        else:
            initHashSet[T]()
    let b =
        if key in self.primal:
            self.primal[key]
        else:
            initHashSet[T]()
    result = (a, b)
    for e in result[0]:
        self.remove((e, key))
    for e in result[1]:
        self.remove((key, e))
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
                for ee in self.primal[e].items:
                    indegree[ee] = indegree[ee] - 1
            indegree.del(e)
proc SCC*[T](self: Order[T]): seq[seq[T]] =
    var
        visited: HashSet[T]
        order: seq[T]
        ret: seq[seq[T]] = @[]

    proc ordering(self: Order[T], node: T) =
        if node in visited:
            return
        visited.incl node

        if self.outdegree(node) != 0:
            for target in self.primal[node]:
                self.ordering(target)

        order.add node

    for node in self.nodes:
        if node notin visited:
            self.ordering(node)

    visited.clear
    proc collect(self: Order[T], node: T) =
        if node in visited:
            return
        visited.incl node
        ret[^1].add node

        if self.indegree(node) != 0:
            for target in self.dual[node]:
                self.collect(target)


    for node in order.reversed:
        if node notin visited:
            ret.add @[]
            self.collect(node)

    ret

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
    result.add "node [\n  shape = none\n];\n"
    result.add "edge [\n  dir = back\n];\n"
    for n in self.nodes.items:
        result.add &"\"{n}\"\n"
    # for key in self.dual.keys:
        # for v in self.dual[key].items:
    for (key, d) in self.dual.pairs:
        for v in d:
            result.add &"\"{key}\" -> \"{v}\"\n"
    result = result[0..^2]
    result = result.indent(2)
    result = &"digraph order {{\n{result}\n}}"

proc dot*[T](self: Order[T], filename: string) =
    let
        f = open(filename, FileMode.fmWrite)
        s = &"```dot\n{self.dot}\n```"
    defer:
        close f
    f.write(s)

when isMainModule:
    var
        order: Order[int] = newOrder[int]()

    order.add((1, 2))
    order.add((2, 3))
    order.add((3, 1))
    order.add((4, 5))
    order.add((5, 4))
    order.add((1, 5))
    order.add((6, 7))

    assert order.SCC() == @[@[6], @[7], @[3, 2, 1], @[4, 5]]
