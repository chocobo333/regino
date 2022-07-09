
import options
import sequtils

import ir
import projects
import errors


proc check*(self: Expression, project: Project, global: bool = false): bool =
    template assume(ty: typed): untyped =
        if self.typ != ty:
            project.errs.add Error.InternalError
            true
        else:
            false
    case self.kind
    of ExpressionKind.Literal:
        case self.litval.kind
        of LiteralKind.Unit:
            assume(Type.Unit)
        of LiteralKind.Bool:
            assume(Type.Bool)
        of LiteralKind.Integer:
            assume(Type.Integer(self.litval.intbits))
        of LiteralKind.Float:
            assume(Type.Integer(self.litval.floatbits))
        of LiteralKind.Char:
            assume(Type.Char)
        of LiteralKind.CString:
            assume(Type.CString)
    of ExpressionKind.Ident:
        if not self.typ.symbol.isSome:
            project.errs.add Error.NotDeclared
            true
        else:
            false
    of ExpressionKind.Call:
        self.callee.check(project, global) or self.args.anyIt(it.check(project, global))
    of ExpressionKind.Apply:
        false
    of ExpressionKind.If:
        false
    of ExpressionKind.Case:
        false
    of ExpressionKind.Pair:
        false
    of ExpressionKind.Array:
        false
    of ExpressionKind.Record:
        false
    of ExpressionKind.ObjCons:
        false
    of ExpressionKind.Ref:
        false
    of ExpressionKind.Import:
        false
    of ExpressionKind.LetSection:
        false
    of ExpressionKind.VarSection:
        false
    of ExpressionKind.TypeSection:
        false
    of ExpressionKind.Assign:
        false
    of ExpressionKind.Funcdef:
        false
    of ExpressionKind.ImportLL:
        false
    of ExpressionKind.Loop:
        false
    of ExpressionKind.Discard:
        false
    of ExpressionKind.Seq:
        false
    of ExpressionKind.Typeof:
        false
    of ExpressionKind.Malloc:
        false
    of ExpressionKind.Realloc:
        false
    of ExpressionKind.PtrSet:
        false
    of ExpressionKind.PtrGet:
        false
