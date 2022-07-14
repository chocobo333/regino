
import tables
import sequtils

import ir


proc compilable*(self: Value): bool =
    case self.kind
    of ValueKind.Unit:
        true
    of ValueKind.Univ:
        false
    of ValueKind.Bool:
        true
    of ValueKind.Integer:
        true
    of ValueKind.Float:
        true
    of ValueKind.Char:
        true
    of ValueKind.CString:
        true
    of ValueKind.Array:
        true
    of ValueKind.Function:
        true
proc compilable*(self: Type): bool =
    case self.kind
    of TypeKind.Value:
        self.val.compilable
    of TypeKind.Univ:
        false
    of TypeKind.Bottom:
        false
    of TypeKind.Unit:
        true
    of TypeKind.Bool..TypeKind.CString:
        true
    of TypeKind.Ptr:
        self.pointee.compilable
    of TypeKind.Pair:
        self.first.compilable and self.second.compilable
    of TypeKind.Array:
        self.base.compilable
    of TypeKind.Record, TypeKind.Object:
        toSeq(self.members.values).all(compilable)
    of TypeKind.Arrow:
        self.params.all(compilable) and self.rety.compilable
    # of TypeKind.Sum:
    #     false
    of TypeKind.Trait:
        false
    # of TypeKind.Sigma:
    #     self.first.compilable and self.second.compilable
    of TypeKind.Intersection, TypeKind.Union, TypeKind.Select:
        false
    of TypeKind.Distinct:
        self.base.compilable
    # of TypeKind.Lambda:
    #     false
    of TypeKind.Recursive:
        true
    of TypeKind.RecursiveVar:
        true
    of TypeKind.Cons:
        true
    of TypeKind.Link:
        self.to.compilable
    of TypeKind.Var:
        assert false, getStackTrace()
        false
    of TypeKind.Singleton:
        false
    of TypeKind.Gen:
        # assert false
        self.gt.ub.compilable
