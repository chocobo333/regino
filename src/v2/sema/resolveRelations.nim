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
    
