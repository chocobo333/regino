
import ../il



import sequtils
import options
import sets
import tables
import strformat
import sugar
import algorithm

# import resolveRelations

import ../il
import ../typeenv
import ../orders
import ../errors
import ../utils

import macros


proc coerceRelation*(self: TypeEnv, t1, t2: Value) =
    # coerce the relation for t1 <= t2
    if t1.kind == t2.kind and t1.kind in {ValueKind.Bottom..ValueKind.Bool, ValueKind.Char, ValueKind.String}:
        discard
    elif t1.kind == t2.kind and t1.kind in {ValueKind.Integer, ValueKind.Float} and t1.bits == t2.bits:
        discard
    else:
        self.constraints.add (t1, t2)
    if t1.kind == t2.kind:
        case t1.kind
        of ValueKind.Pair:
            self.coerceRelation(t1.first, t2.first)
            self.coerceRelation(t1.second, t2.second)
        of ValueKind.Array, ValueKind.Singleton:
            self.coerceRelation(t1.base, t2.base)
        of ValueKind.ArrayV:
            for (t1, t2) in t1.vals.zip(t2.vals):
                self.coerceRelation(t1, t2)
        of ValueKind.Record:
            for (id, t2) in t2.members.pairs:
                self.coerceRelation(t1.members.getOrDefault(id, Value.Unit), t2)
        of ValueKind.Ptr:
            self.coerceRelation(t1.pointee, t2.pointee)
        of ValueKind.Pi:
            self.coerceRelation(t1.rety, t2.rety)
            for (t1, t2) in t1.params.zip(t2.params):
                self.coerceRelation(t2, t1)
        of ValueKind.Link:
            self.coerceRelation(t1.to, t2.to)
        else:
            discard
    if t1.kind == ValueKind.Distinct:
        self.coerceRelation(t1.base, t2)
proc coerceEq*(self: TypeEnv, t1, t2: Value) =
    self.coerceRelation(t1, t2)
    self.coerceRelation(t2, t1)

proc addTypeRelation*(self: TypeEnv, t1, t2: Value, fn: Ident) =
    # setTypeEnv(self)
    # assert t1.kind == ValueKind.Typedesc
    # assert t2.kind == ValueKind.Typedesc
    # assert not (t2.`typedesc` <= t1.`typedesc`)
    self.scope.typeOrder.add (t1, t2)
    self.scope.converters[(t1, t2)] = fn

macro coerce*(self: TypeEnv, rel: untyped): untyped =
    rel.expectKind(nnkInfix)
    let
        (op, l, r) = (rel[0], rel[1], rel[2])
    if op.strVal == "==":
        quote:
            `self`.coerceEq(`l`, `r`)
    elif op.strVal == "<=":
        quote:
            `self`.coerceRelation(`l`, `r`)
    else:
        error fmt"permitted is {l} == {r} or {l} <= {r}.", op
        newEmptyNode()
