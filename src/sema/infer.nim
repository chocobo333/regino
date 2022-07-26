
import tables
import sequtils
import options

import ir
import projects
import coerce


proc infer*(self: Expression, project: Project, global: bool = false) =
    # collect constraints of types
    proc infer(self: IdentDef, project: Project, global: bool = false) =
        if self.default.isSome:
            project.env.coerce(self.pat.typ <= self.default.get.typ)
    case self.kind
    of ExpressionKind.Literal:
        discard
    of ExpressionKind.Ident:
        discard
    of ExpressionKind.Call:
        self.callee.infer(project, global)
        for e in self.args:
            e.infer(project, global)
        project.env.coerce(self.callee.typ <= Type.Arrow(self.args.mapIt(it.typ), self.typ))
        project.env.coerce(Type.Arrow(self.args.mapIt(Type.Unit), self.typ) <= self.callee.typ)
    of ExpressionKind.Apply:
        discard
    of ExpressionKind.If:
        self.cond.infer(project, global)
        self.then.infer(project, global)
        self.els.infer(project, global)
        project.env.coerce(self.cond.typ <= Type.Bool)
        project.env.coerce(self.then.typ <= self.typ)
        project.env.coerce(self.els.typ <= self.typ)
    of ExpressionKind.Case:
        # TODO:
        discard
    of ExpressionKind.Pair:
        self.first.infer(project, global)
        self.second.infer(project, global)
    of ExpressionKind.Array:
        for e in self.elements:
            e.infer(project, global)
            project.env.coerce(e.typ <= self.typ)
    of ExpressionKind.Record:
        for (k, v) in self.members.pairs:
            v.infer(project, global)
    of ExpressionKind.ObjCons:
        # TODO:
        discard
    of ExpressionKind.Ref:
        self.to.infer(project, global)
    of ExpressionKind.FnType:
        # TODO:
        discard
    of ExpressionKind.Import:
        # TODO:
        discard
    of ExpressionKind.Link:
        # TODO:
        discard
    of ExpressionKind.LetSection, ExpressionKind.VarSection:
        for iddef in self.iddefs:
            iddef.infer(project, global)
    of ExpressionKind.ConstSection:
        discard
    of ExpressionKind.TypeSection:
        discard
    of ExpressionKind.Assign:
        project.env.coerce(self.assign_lval.typ <= self.assign_val.typ)
    of ExpressionKind.Funcdef:
        self.fn.body.infer(project, global)
        project.env.coerce(self.fn.body.typ <= self.fn.signature.ident.typ)
    of ExpressionKind.ImportLL:
        discard
    of ExpressionKind.Loop:
        self.`block`.infer(project, global)
    of ExpressionKind.Discard:
        self.`block`.infer(project, global)
    of ExpressionKind.Seq:
        for e in self.expressions:
            e.infer(project, global)
    of ExpressionKind.Typeof:
        self.typeof.infer(project, global)
    of ExpressionKind.Malloc:
        self.msize.infer(project, global)
        project.env.coerce(self.msize.typ <= Type.Integer(64))
    of ExpressionKind.Realloc:
        self.rptr.infer(project, global)
        self.msize.infer(project, global)
        project.env.coerce(self.msize.typ <= Type.Integer(64))
    of ExpressionKind.PtrSet:
        self.`ptr`.infer(project, global)
        self.index.infer(project, global)
        self.val.infer(project, global)
    of ExpressionKind.PtrGet:
        self.`ptr`.infer(project, global)
        self.index.infer(project, global)
        project.env.coerce(self.`ptr`.typ == Type.Ptr(self.typ))
