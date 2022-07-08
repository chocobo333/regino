
import sets
import sequtils

import ir
import projects
import typeenvs
import coerce


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
proc infer(self: Expression, project: Project, global: bool = false): Type =
    case self.kind
    of ExpressionKind.Literal:
        self.litval.infer()
    of ExpressionKind.Ident:
        self.ident.infer(project)
    of ExpressionKind.Call:
        let
            tv = Type.Var(project.env)
            callee = self.callee.infer(project, global)
            args = self.args.mapIt(it.infer(project, global))
        project.env.coerce(callee <= Type.Arrow(args, tv))
        project.env.coerce(Type.Arrow(args.mapIt(Type.Unit), tv) <= callee) # i dont know whether this is correct.
        tv
    of ExpressionKind.Apply:
        let
            callee = self.callee.infer(project, global)
        Type.Unit
    of ExpressionKind.If:
        let
            tv = Type.Var(project.env)
            cond = self.cond.infer(project, global)
            then = self.then.infer(project, global)
            els = self.els.infer(project, global)
        project.env.coerce(cond <= Type.Bool)
        project.env.coerce(then <= tv)
        project.env.coerce(els <= tv)
        tv
    of ExpressionKind.Case:
        # TODO:
        Type.Unit
    of ExpressionKind.Pair:
        let
            first = self.first.infer(project, global)
            second = self.second.infer(project, global)
        Type.Pair(first, second)
    of ExpressionKind.Array:
        let
            tv = Type.Var(project.env)
            elements = self.elements.mapIt(it.infer(project, global))
        for e in elements:
            project.env.coerce(e <= tv)
        tv
    of ExpressionKind.Record:
        # TODO:
        Type.Unit
    of ExpressionKind.ObjCons:
        # TODO:
        Type.Unit
    of ExpressionKind.Ref:
        Type.Ptr(self.to.infer(project, global))
    of ExpressionKind.Import:
        # TODO:
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
