
import ir



import sequtils
import sets
import tables
import strformat

# import resolveRelations

import typeenvs
import ../orders

import macros

import ../utils


proc coerceRelation*(self: TypeEnv, t1, t2: Type) =
    # coerce the relation for t1 <= t2
    if t1.kind == TypeKind.Link:
        self.coerceRelation(t1.to, t2)
        return
    if t2.kind == TypeKind.Link:
        self.coerceRelation(t1, t2.to)
        return
    if t1.kind == t2.kind and t1.kind in {TypeKind.Bottom..TypeKind.Bool, TypeKind.Char, TypeKind.CString}:
        discard
    elif t1.kind == t2.kind and t1.kind in {TypeKind.Integer, TypeKind.Float} and t1.nbits == t2.nbits:
        discard
    else:
        self.constraints.add (t1, t2)
    if t1.kind == t2.kind:
        case t1.kind
        of TypeKind.Pair:
            self.coerceRelation(t1.first, t2.first)
            self.coerceRelation(t1.second, t2.second)
        of TypeKind.Array, TypeKind.Singleton:
            self.coerceRelation(t1.base, t2.base)
        of TypeKind.Record:
            for (id, t2) in t2.members.pairs:
                self.coerceRelation(t1.members.getOrDefault(id, Type.Unit), t2)
        of TypeKind.Ptr:
            self.coerceRelation(t1.pointee, t2.pointee)
            self.coerceRelation(t2.pointee, t1.pointee)
        of TypeKind.Arrow:
            self.coerceRelation(t1.rety, t2.rety)
            for (t1, t2) in t1.params.zip(t2.params):
                self.coerceRelation(t2, t1)
        of TypeKind.Link:
            self.coerceRelation(t1.to, t2.to)
        else:
            discard
    if t1.kind == TypeKind.Distinct:
        self.coerceRelation(t1.base, t2)
proc coerceEq*(self: TypeEnv, t1, t2: Type) =
    self.coerceRelation(t1, t2)
    self.coerceRelation(t2, t1)

proc addTypeRelation*(self: TypeEnv, t1, t2: Type, fn: Ident) =
    # setTypeEnv(self)
    # assert t1.kind == TypeKind.Typedesc
    # assert t2.kind == TypeKind.Typedesc
    # assert not (t2.`typedesc` <= t1.`typedesc`)
    # self.scope.typeOrder.add (t1, t2)
    # self.scope.converters[(t1, t2)] = fn
    # TODO: add type relation
    discard

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
