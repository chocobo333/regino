
import tables

import ir


proc `==`*(self, other: GenericType): bool =
    self.id == other.id

proc `==`*(self, other: Value): bool =
    # TODO:
    if self.kind == other.kind:
        case self.kind:
        of ValueKind.Unit:
            true
        of ValueKind.Univ:
            self.level == other.level
        of ValueKind.Bool:
            self.boolval == other.boolval
        of ValueKind.Integer:
            self.intval == other.intval and self.intbits == other.intbits
        of ValueKind.Float:
            self.floatval == other.floatval and self.floatbits == other.floatbits
        of ValueKind.Char:
            self.charval == other.charval
        of ValueKind.CString:
            self.strval == other.strval
        of ValueKind.Array:
            self.vals == other.vals
        of ValueKind.Function:
            false
    else:
        false

proc `==`*(self, other: Type): bool =
    if self.kind == TypeKind.Link:
        self.to == other
    elif other.kind == TypeKind.Link:
        self == other.to
    elif self.kind == other.kind:
        case self.kind
        of TypeKind.Bottom, TypeKind.Unit:
            true
        of TypeKind.Univ:
            self.level == other.level
        of TypeKind.Value:
            self.val == other.val
        of TypeKind.Bool:
            true
        of TypeKind.Integer, TypeKind.Float:
            self.nbits == other.nbits
        of TypeKind.Char, TypeKind.CString:
            true
        of TypeKind.Pair:
            self.first == other.first and self.second == other.second
        of TypeKind.Array:
            self.length == other.length and self.base == other.base
        of TypeKind.Record:
            self.members == other.members
        of TypeKind.Object:
            self.members == other.members
        of TypeKind.Arrow:
            self.params == other.params and self.rety == other.rety
        of TypeKind.Cons:
            self.cons == other.cons and self.args == other.args
        of TypeKind.Distinct, TypeKind.Singleton:
            self.base == other.base
        of TypeKind.Ptr:
            self.pointee == other.pointee
        of TypeKind.Recursive:
            self.self == other.self and self.body == other.body
        of TypeKind.Trait:
            false
            # self.paty == other.paty and
            # self.iss == other.iss and
            # self.fns == other.fns and
            # self.fnss == other.fnss
        of TypeKind.Var, TypeKind.Select, TypeKind.RecursiveVar:
            self.id == other.id
        of TypeKind.Intersection, TypeKind.Union:
            self.types == other.types
        of TypeKind.Gen:
            self.gt.id == other.gt.id
        of TypeKind.Link:
            self.to == other.to
    else:
        false
