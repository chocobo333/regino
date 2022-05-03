
import orders
import tables
import strformat
import strutils
import os

type
    Dot[T] = object
        dots: seq[string]

    DotElement[T] = object
        nodeAttr: Table[string, string]
        edgeAttr: Table[string, string]
        graphAttr: Table[string, string]
        order: Order[T]
        focusedNode: T
        focusedEdge: (T, T)

proc newDot*[T](): Dot[T] =
    Dot[T]()

proc nodeAttr2string(self: Table[string, string]): string =
    for (key, val) in self.pairs:
        result.add fmt"{key} = {val}"
    result = result.indent(2)
    result = &"node [\n{result}];\n"
proc edgeAttr2string(self: Table[string, string]): string =
    for (key, val) in self.pairs:
        result.add fmt"{key} = {val}"
    result = result.indent(2)
    result = &"edge [\n{result}];\n"

proc to*[T](self: DotElement[T]): string =
    result.add nodeAttr2string(self.nodeAttr)
    result.add nodeAttr2string(self.edgeAttr)

    for n in self.order.nodes.items:
        result.add &"\"{n}\"\n"
    for (key, d) in self.order.dual.pairs:
        for v in d:
            result.add &"\"{key}\" -> \"{v}\"\n"
    result = result[0..^2]
    result = result.indent(2)
    result = &"digraph order {{\n{result}\n}}"

proc add*[T](self: var Dot[T], order: Order[T]) =
    self.dots.add DotElement[T](order: order).to

proc focus*[T](self: Dot[T], n: T) =
    self.dots[^1].focusedNode = n
proc focus*[T](self: Dot[T], e: (T, T)) =
    self.dots[^1].focusedEdge = e
proc save*[T](self: Dot[T], folder: string) =
    discard execShellCmd(fmt"mkdir -p {folder}")
    for (i, e) in self.dots.pairs:
        let f = open(fmt"{folder}/{i}.dot", fmWrite)
        defer:
            f.close
        f.write e
