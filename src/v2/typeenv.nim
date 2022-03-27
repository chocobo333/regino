
import sets

import il


type
    TypeEnv* = object
        scope*: Scope
        tvs*: HashSet[Value]
        constraints*: seq[Constraint]
        tvconstraints*: seq[Constraint]
        interconstraints*: seq[Constraint]
    Constraint* = (Value, Value)   # for t1 <= t2
