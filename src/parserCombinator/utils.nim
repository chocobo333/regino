
import strutils
import strformat

import concepts

import coloredString


proc indent*[T: To[string]](a: T): string =
    ($a).indent(2)

proc gray(s: To[string]): string = 
    &"\e[38;5;{0xf8}m{s}\e[00m"

proc genGraph*[T: To[string]](parent: T, sons: varargs[string, `$`]): string =
    let
        sons = @sons
        l = sons.len
    result = $parent
    if l == 0:
        return
    for i in 0..<l-1:
        var tmp = indent($sons[i], 1, "┇  ".gray)
        tmp[0..<"┇  ".gray.len] = "┣━ ".gray
        result &= "\n" & tmp
    var tmp = indent($sons[^1], 1, "   ")
    tmp[0..<2] = "┗━".gray
    result &= "\n" & tmp

proc genGraphS*[T: To[string]](parent: string, sons: seq[T]): string =
    let
        sons = @sons
        l = sons.len
    result = $parent
    if l == 0:
        return
    for i in 0..<l-1:
        var tmp = indent($sons[i], 1, "┇  ".gray)
        tmp[0..<"┇  ".gray.len] = "┣━ ".gray
        result &= "\n" & tmp
    var tmp = indent($sons[^1], 1, "   ")
    tmp[0..<2] = "┗━".gray
    result &= "\n" & tmp


when isMainModule:
    echo genGraph(3, 4, 5)
    echo genGraph(4, 5, genGraph(3, 4, 5))
    echo genGraph(4, genGraph(3, 4, 5), genGraph(3, 4, 5))
    echo genGraph(4, genGraph(3, genGraph(3, 4, 5), genGraph(3, 4, 5)), genGraph(3, 4, 5))
    echo genGraph(4, genGraph(2, genGraph(3, genGraph(3, 4, 5), 5), genGraph(3, 4, 5), genGraph(3, 4, 5)), genGraph(3, 4, 5))
    echo genGraph(4, genGraph(2, genGraph(3, genGraph(3, 4, 5), 5), genGraph(3, 4, 5), genGraph(3, 4, genGraph(3, 4, 5))), genGraph(3, genGraph(3, 4, 5), 5))