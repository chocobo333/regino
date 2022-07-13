
import sets
import tables
import sequtils
import sugar

import ir
import projects
import typeenvs
import coerce

import ../utils


proc infer(self: Literal): Type =
    self.typ
proc infer*(self: Expression, project: Project, global: bool = false): Type =
    # collect constraints of types
    case self.kind
    of ExpressionKind.Literal:
        self.litval.infer()
    of ExpressionKind.Ident:
        self.typ
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
        var
            members = initTable[string, Type]()
        for (k, v) in self.members.pairs:
            members[k.name] = v.infer(project, global)
        Type.Record(members)
    of ExpressionKind.ObjCons:
        # TODO:
        Type.Unit
    of ExpressionKind.Ref:
        Type.Ptr(self.to.infer(project, global))
    of ExpressionKind.Import:
        # TODO:
        Type.Unit
    of ExpressionKind.LetSection:
        # TODO:
        Type.Unit
    of ExpressionKind.VarSection:
        # TODO:
        Type.Unit
    of ExpressionKind.ConsSection:
        # TODO:
        Type.Unit
    of ExpressionKind.TypeSection:
        # TODO:
        Type.Unit
    of ExpressionKind.Assign:
        # TODO:
        Type.Unit
    of ExpressionKind.Funcdef:
        # TODO:
        Type.Unit
    of ExpressionKind.ImportLL:
        # TODO:
        Type.Unit
    of ExpressionKind.Loop:
        # TODO:
        Type.Unit
    of ExpressionKind.Discard:
        # TODO:
        Type.Unit
    of ExpressionKind.Seq:
        # TODO:
        Type.Unit
    of ExpressionKind.Typeof:
        # TODO:
        Type.Unit
    of ExpressionKind.Malloc:
        # TODO:
        Type.Unit
    of ExpressionKind.Realloc:
        # TODO:
        Type.Unit
    of ExpressionKind.PtrSet:
        # TODO:
        Type.Unit
    of ExpressionKind.PtrGet:
        # TODO:
        Type.Unit
