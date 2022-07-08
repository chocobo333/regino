
import sets
import sequtils

import ir
import projects
import typeenvs


proc infer(self: Ident, project: Project, global: bool = false): Type =
    let
        name = self.name
        vars = project.env.lookupVar(name)
        types = project.env.lookupType(name)
        funcs = project.env.lookupFunc(name)
        t = vars.mapIt(it.typ) & types.mapIt(it.typ) & funcs.mapIt(it.pty.inst)
    case t.len
    of 0:
        let
            tv = Type.Var(project.env)
            symbol = Symbol.NotDeclared(self, tv, global)
        project.addErr project.env.addSymbol(symbol)
        tv
    of 1:
        t[0]
    else:
        Type.Select(t, project.env)

proc infer(self: Literal): Type =
    case self.kind
    of LiteralKind.Unit:
        Type.Unit
    of LiteralKind.Integer:
        Type.Integer(self.intbits)
    of LiteralKind.Float:
        Type.Float(self.floatbits)
    of LiteralKind.Char:
        Type.Char
    of LiteralKind.CString:
        Type.CString
proc infer(self: Expression, project: Project, global: bool = false): Type =
    case self.kind
    of ExpressionKind.Literal:
        self.litval.infer()
    of ExpressionKind.Ident:
        self.ident.infer(project)
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
