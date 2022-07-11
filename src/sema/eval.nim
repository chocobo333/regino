
import sequtils
import options

import ir
import projects
import typeenvs
import infer
import check

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

proc eval*(self: Expression, project: Project): Type
proc preeval*(self: Expression, project: Project): Type =
    # compile-time evaluation
    result = case self.kind
    of ExpressionKind.Literal:
        self.litval.eval(project)
    of ExpressionKind.Ident:
        # TODO:
        Type.Bottom
    of ExpressionKind.Call:
        discard self.callee.preeval(project)
        discard self.args.mapIt(it.preeval(project))
        Type.Var(project.env)
    of ExpressionKind.Apply:
        # TODO: type
        Type.Bottom
    of ExpressionKind.If:
        Type.Bottom
    of ExpressionKind.Case:
        # TODO: Pattern
        Type.Bottom
    of ExpressionKind.Pair:
        Type.Bottom
    of ExpressionKind.Array:
        Type.Bottom
    of ExpressionKind.Record:
        Type.Bottom
    of ExpressionKind.ObjCons:
        let
            obj = self.obj.eval(project)
            implicits = self.implicits.mapIt(it.eval(project))
        Type.Bottom
    of ExpressionKind.Ref:
        Type.Bottom
    of ExpressionKind.Import:
        Type.Bottom
    of ExpressionKind.LetSection:
        Type.Bottom
    of ExpressionKind.VarSection:
        Type.Bottom
    of ExpressionKind.TypeSection:
        Type.Bottom
    of ExpressionKind.Assign:
        Type.Bottom
    of ExpressionKind.Funcdef:
        Type.Bottom
    of ExpressionKind.ImportLL:
        Type.Bottom
    of ExpressionKind.Loop:
        Type.Bottom
    of ExpressionKind.Discard:
        Type.Bottom
    of ExpressionKind.Seq:
        Type.Bottom
    of ExpressionKind.Typeof:
        Type.Bottom
    of ExpressionKind.Malloc:
        Type.Bottom
    of ExpressionKind.Realloc:
        Type.Bottom
    of ExpressionKind.PtrSet:
        Type.Bottom
    of ExpressionKind.PtrGet:
        Type.Bottom
    self.typ = result
proc posteval*(self: Expression, project: Project): Type =
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

proc eval*(self: Expression, project: Project): Type =
    discard self.preeval(project)
    discard self.infer(project)
    discard self.check(project)
    discard self.posteval(project)
