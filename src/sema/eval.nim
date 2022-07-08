
import options

import ir
import projects

proc eval(self: Expression, project: Project): Type =
    case self.kind
    of ExpressionKind.Unit:
        Type.Value(ir.Value.Unit)
    of ExpressionKind.Integer:
        Type.Value(ir.Value.Integer(self.intval, self.intbits))
    of ExpressionKind.Float:
        Type.Value(ir.Value.Float(self.floatval, self.floatbits))
    of ExpressionKind.Char:
        Type.Value(ir.Value.Char(self.charval))
    of ExpressionKind.CString:
        Type.Value(ir.Value.CString(self.strval))
    of ExpressionKind.Ident:
        self.typ.symbol.get.val
    of ExpressionKind.Call:
        Type.Unit
    of ExpressionKind.Apply:
        Type.Unit
    of ExpressionKind.If:
        Type.Unit
    of ExpressionKind.Case:
        Type.Unit
    of ExpressionKind.ObjCons:
        Type.Unit
    of ExpressionKind.Ref:
        Type.Unit
    of ExpressionKind.Import:
        Type.Unit
    of ExpressionKind.LetSection:
        Type.Unit
    of ExpressionKind.VarSection:
        Type.Unit
    of ExpressionKind.TypeSection:
        Type.Unit
    of ExpressionKind.Assign:
        Type.Unit
    of ExpressionKind.Funcdef:
        Type.Unit
    of ExpressionKind.ImportLL:
        Type.Unit
    of ExpressionKind.Loop:
        Type.Unit
    of ExpressionKind.Discard:
        Type.Unit
    of ExpressionKind.Seq:
        Type.Unit
    of ExpressionKind.Typeof:
        Type.Unit
    of ExpressionKind.Malloc:
        Type.Unit
    of ExpressionKind.Realloc:
        Type.Unit
    of ExpressionKind.PtrSet:
        Type.Unit
    of ExpressionKind.PtrGet:
        Type.Unit
