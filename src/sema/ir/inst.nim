
import tables
import sets
import sequtils
import sugar

import ir
import constructors
import hash
import tos

import ../../utils


proc inst*(self: Type, subs: Table[GenericType, Type]): Type =
    let symbol = self.symbol
    result = case self.kind
    of TypeKind.Bottom:
        Type.Bottom()
    of TypeKind.Unit:
        Type.Unit()
    of TypeKind.Univ:
        Type.Univ(self.level)
    of TypeKind.Value:
        Type.value(self.val)
    of TypeKind.Bool:
        Type.Bool()
    of TypeKind.Integer:
        Type.Integer(self.nbits)
    of TypeKind.Float:
        Type.Float(self.nbits)
    of TypeKind.Char:
        Type.Char()
    of TypeKind.CString:
        Type.CString()
    of TypeKind.Pair:
        Type.Pair(self.first.inst(subs), self.second.inst(subs))
    of TypeKind.Array:
        Type.Array(self.length, self.base.inst(subs))
    of TypeKind.Record:
        Type.Record(self.members.map(it => inst(it, subs)))
    of TypeKind.Object:
        Type.Object(self.members.map(it => inst(it, subs)))
    of TypeKind.Arrow:
        Type.Arrow(self.params.map(it => inst(it, subs)), self.rety.inst(subs))
    of TypeKind.Cons:
        Type.Cons(self.cons, self.args.map(it => inst(it, subs)))
    of TypeKind.Distinct:
        Type.Distinct(self.base.inst(subs))
    of TypeKind.Singleton:
        Type.Singleton(self.base.inst(subs))
    of TypeKind.Ptr:
        Type.Ptr(self.pointee.inst(subs))
    of TypeKind.Recursive:
        Type.Recursive(self.self, self.body.inst(subs))
    of TypeKind.Trait:
        Type.Trait((self.paty[0], self.paty[1].inst(subs)), self.iss, self.fns, self.fnss)
    of TypeKind.Var, TypeKind.Select, TypeKind.RecursiveVar:
        Type.Link(self)
    of TypeKind.Intersection:
        var types = initHashSet[Type]()
        for e in self.types:
            types.incl e.inst(subs)
        Type.Intersection(types)
    of TypeKind.Union:
        var types = initHashSet[Type]()
        for e in self.types:
            types.incl e.inst(subs)
        Type.Union(types)
    of TypeKind.Gen:
        debug subs
        if self.gt in subs: subs[self.gt] else: Type.Gen(self.gt)
    of TypeKind.Link:
        Type.Link(self.to)
    result.symbol = symbol

proc inst*(self: PiType, p: seq[Type]): Type =
    assert self.params.len == p.len
    var
        subs = initTable[GenericType, Type]()
    for (a, b) in self.params.zip(p):
        subs[a] = b
    self.rety.inst(subs)

proc gen*(self: Type): PiType =
    # TODO:
    PiType(
        rety: self,
    )
