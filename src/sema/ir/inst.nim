
import tables
import sequtils
import sugar

import ir
import constructors
import hash

import ../../utils


proc inst*(self: Type, subs: Table[GenericType, Type]): Type =
    case self.kind
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
    of TypeKind.Var:
        Type.Var(self.tv)
    of TypeKind.Gen:
        if self.gt in subs: subs[self.gt] else: Type.Gen(self.gt)
    of TypeKind.Link:
        Type.Link(self)
