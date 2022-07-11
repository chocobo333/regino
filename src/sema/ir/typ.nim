
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
