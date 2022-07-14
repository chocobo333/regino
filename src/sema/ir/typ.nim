
import ir
import constructors


proc typ*(self: Literal): Type =
    case self.kind
    of LiteralKind.Unit:
        Type.Unit
    of LiteralKind.Bool:
        Type.Bool
    of LiteralKind.Integer:
        Type.Integer(self.intbits)
    of LiteralKind.Float:
        Type.Float(self.floatbits)
    of LiteralKind.Char:
        Type.Char
    of LiteralKind.CString:
        Type.CString
    of LiteralKind.Univ:
        Type.Univ(self.level)

proc typ*(self: Value): Type =
    # TODO:
    case self.kind
    of ValueKind.Unit:
        Type.Unit
    of ValueKind.Univ:
        Type.Univ(self.level + 1)
    of ValueKind.Bool:
        Type.Bool
    of ValueKind.Integer:
        Type.Integer(self.intbits)
    of ValueKind.Float:
        Type.Float(self.floatbits)
    of ValueKind.Char:
        Type.Char
    of ValueKind.CString:
        Type.CString
    of ValueKind.Array:
        Type.Univ(0)
    of ValueKind.Function:
        Type.Univ(0)

proc typ*(self: Type): Type =
    # TODO:
    case self.kind
    of TypeKind.Bottom:
        Type.Univ(0)
    of TypeKind.Unit:
        Type.Univ(0)
    of TypeKind.Univ:
        Type.Univ(self.level + 1)
    of TypeKind.Value:
        self.val.typ
    of TypeKind.Bool:
        Type.Univ(0)
    of TypeKind.Integer:
        Type.Univ(0)
    of TypeKind.Float:
        Type.Univ(0)
    of TypeKind.Char:
        Type.Univ(0)
    of TypeKind.CString:
        Type.Univ(0)
    of TypeKind.Pair:
        Type.Univ(0)
    of TypeKind.Array:
        Type.Univ(0)
    of TypeKind.Record:
        Type.Univ(0)
    of TypeKind.Object:
        Type.Univ(0)
    of TypeKind.Arrow:
        Type.Univ(0)
    of TypeKind.Cons:
        Type.Univ(0)
    of TypeKind.Distinct:
        Type.Univ(0)
    of TypeKind.Singleton:
        Type.Univ(0)
    of TypeKind.Ptr:
        Type.Univ(0)
    of TypeKind.Recursive:
        Type.Univ(0)
    of TypeKind.Trait:
        Type.Univ(0)
    of TypeKind.Var:
        Type.Univ(0)
    of TypeKind.Select:
        Type.Univ(0)
    of TypeKind.RecursiveVar:
        Type.Univ(0)
    of TypeKind.Intersection:
        Type.Univ(0)
    of TypeKind.Union:
        Type.Univ(0)
    of TypeKind.Gen:
        Type.Univ(0)
    of TypeKind.Link:
        Type.Univ(0)

proc typ*(self: PiType): Type =
    # TODO:
    Type.Univ(1)
