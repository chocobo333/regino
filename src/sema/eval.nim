
import options

import ir
import projects

proc eval(self: Literal, project: Project): Type =
    case self.kind
    of LiteralKind.Unit:
        Type.value(Value.Unit)
    of LiteralKind.Bool:
        Type.value(Value.Bool(self.boolval))
    of LiteralKind.Integer:
        Type.value(Value.Integer(self.intval, self.intbits))
    of LiteralKind.Float:
        Type.value(Value.Float(self.floatval, self.floatbits))
    of LiteralKind.Char:
        Type.value(Value.Char(self.charval))
    of LiteralKind.CString:
        Type.value(Value.CString(self.strval))

proc preeval(self: Expression, project: Project): Type =
    case self.kind
    of ExpressionKind.Literal:
        self.litval.eval(project)
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
    of ExpressionKind.Pair:
        Type.Unit
    of ExpressionKind.Array:
        Type.Unit
    of ExpressionKind.Record:
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
proc eval(self: Expression, project: Project): Type =
    case self.kind
    of ExpressionKind.Literal:
        self.litval.eval(project)
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
    of ExpressionKind.Pair:
        Type.Unit
    of ExpressionKind.Array:
        Type.Unit
    of ExpressionKind.Record:
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
