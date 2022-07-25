
import tables
import sequtils
import options

import ir
import projects
import typeenvs
import infer
import check
import coerce
import resolve

import ../utils


proc eval(self: Literal, project: Project): Type =
    case self.kind
    of LiteralKind.Unit:
        Type.value(Value.Unit)
    of LiteralKind.Univ:
        Type.value(Value.Univ(self.level))
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

proc eval*(self: Expression, project: Project, global: bool = false): Type
proc eval*(self: TypeExpression, project: Project, global: bool = false): Type =
    # TODO
    case self.kind
    of TypeExpressionKind.Ref:
        Type.Unit
    of TypeExpressionKind.Object:
        Type.Unit
    of TypeExpressionKind.Variant:
        Type.Unit
    of TypeExpressionKind.Trait:
        Type.Unit
    of TypeExpressionKind.Expression:
        self.expression.eval(project, global)
proc predeclare*(self: Expression, project: Project, global: bool = false) =
    proc predeclare(self: IdentDef, project: Project, global: bool = false) =
        if self.default.isSome:
            self.default.get.predeclare(project, global)
    proc predeclare(self: GenTypeDef, project: Project, global: bool = false) =
        let
            tv = Type.Gen(project.env, Type.Var(project.env), Type.Var(project.env))
            symbol = Symbol.GenParam(self.ident, newPiType(tv, @[], some self.ident))
        project.addErr project.env.addSymbol(symbol)
        self.ident.typ = tv
    proc predeclare(self: TypeDef, project: Project, global: bool = false): Symbol =
        for e in self.params:
            e.predeclare(project, global)
        let
            tv = Type.Var(project.env)
            implicits = self.params.mapIt(it.ident.typ.gt)
            pval = newPiType(tv, implicits, some self.ident)
        Symbol.Typ(self.ident, pval, global, self.loc)
    proc predeclare(self: FunctionSignature, project: Project, global: bool = false): Symbol =
        for e in self.implicits:
            e.predeclare(project, global)
        for e in self.params:
            e.predeclare(project, global)
        let
            implicits = self.implicits.mapIt(it.ident.typ.gt)
            tv = Type.Arrow(self.params.mapIt(Type.Var(project.env)), Type.Var(project.env))
            pty = newPiType(tv, implicits, some self.ident)
        Symbol.Func(self.ident, pty, global)
    proc predeclare(self: Function, project: Project, global: bool = false): Symbol =
        project.env.enter self.body.scope:
            result = self.signature.predeclare(project, global)
            self.body.predeclare(project, global)
    case self.kind
    of ExpressionKind.Literal:
        discard
    of ExpressionKind.Ident:
        discard
    of ExpressionKind.Call:
        self.callee.predeclare(project, global)
        for e in self.args:
            e.predeclare(project, global)
    of ExpressionKind.Apply:
        self.callee.predeclare(project, global)
        for e in self.args:
            e.predeclare(project, global)
    of ExpressionKind.If:
        self.cond.predeclare(project, global)
        project.env.enter self.then.scope:
            self.then.predeclare(project, global)
        project.env.enter self.els.scope:
            self.els.predeclare(project, global)
    of ExpressionKind.Case:
        # TODO: Pattern
        discard
    of ExpressionKind.Pair:
        self.first.predeclare(project, global)
        self.second.predeclare(project, global)
    of ExpressionKind.Array:
        for e in self.elements:
            e.predeclare(project, global)
    of ExpressionKind.Record:
        for v in self.members.values:
            v.predeclare(project, global)
    of ExpressionKind.ObjCons:
        # self.obj.predeclare(project, global)
        for e in self.implicits:
            e.predeclare(project, global)
        for v in self.members.values:
            v.predeclare(project, global)
    of ExpressionKind.Ref:
        self.to.predeclare(project, global)
    of ExpressionKind.Import:
        # TODO:
        discard
    of ExpressionKind.LetSection:
        for e in self.iddefs:
            e.predeclare(project, global)
    of ExpressionKind.VarSection:
        for e in self.iddefs:
            e.predeclare(project, global)
    of ExpressionKind.ConstSection:
        # TODO:
        discard
    of ExpressionKind.TypeSection:
        var symbol: Symbol
        project.env.enter self.scope:
            symbol = self.typedef.predeclare(project, global)
        project.addErr project.env.addSymbol(symbol)
    of ExpressionKind.Assign:
        self.assign_val.predeclare(project, global)
    of ExpressionKind.Funcdef:
        let symbol = self.fn.predeclare(project, global)
        project.addErr project.env.addSymbol(symbol)
    of ExpressionKind.ImportLL:
        var symbol: Symbol
        project.env.enter self.scope:
            symbol = self.signature.predeclare(project, global)
        project.addErr project.env.addSymbol(symbol)
    of ExpressionKind.Loop:
        project.env.enter self.`block`.scope:
            self.`block`.predeclare(project, global)
    of ExpressionKind.Discard:
        project.env.enter self.`block`.scope:
            self.`block`.predeclare(project, global)
    of ExpressionKind.Seq:
        for e in self.expressions:
            e.predeclare(project, global)
    of ExpressionKind.Typeof:
        self.`typeof`.predeclare(project, global)
    of ExpressionKind.Malloc:
        self.msize.predeclare(project, global)
        self.mtype.predeclare(project, global)
    of ExpressionKind.Realloc:
        self.msize.predeclare(project, global)
        self.rptr.predeclare(project, global)
    of ExpressionKind.PtrSet:
        self.`ptr`.predeclare(project, global)
        self.index.predeclare(project, global)
        self.val.predeclare(project, global)
    of ExpressionKind.PtrGet:
        self.`ptr`.predeclare(project, global)
        self.index.predeclare(project, global)

proc preeval(self: Ident, project: Project, global: bool = false): Type =
    let
        name = self.name
        vars = project.env.lookupVar(name)
        types = project.env.lookupType(name)
        funcs = project.env.lookupFunc(name)
        t = vars.mapIt(it.typ.inst(project.env)) & types.mapIt(it.typ.inst(project.env)) & funcs.mapIt(it.typ.inst(project.env))
    result = case t.len
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
    self.typ = result
proc preeval*(self: Expression, project: Project, global: bool = false): Type =
    # compile-time evaluation
    # returns self's type
    proc preevalLet(self: Ident, project: Project, global: bool = false): Type =
        let
            tv = Type.Var(project.env)
            symbol = Symbol.Let(self, tv, global)
        project.addErr project.env.addSymbol(symbol)
        tv
    proc preevalLet(self: Pattern, project: Project, global: bool = false): Type =
        result = case self.kind
        of PatternKind.Literal:
            self.litval.typ
        of PatternKind.Ident:
            self.ident.preevalLet(project, global)
        of PatternKind.Tuple:
            # TODO: tag
            self.patterns.mapIt(it.preevalLet(project, global)).foldl(Type.Pair(a, b))
        of PatternKind.Record:
            # TODO: tag
            var members = initTable[string, Type]()
            for (k, v) in self.members:
                discard k.preevalLet(project, global)
                members[k.name] = v.preevalLet(project, global)
            Type.Record(members)
        self.typ = result
    proc preevalLet(self: IdentDef, project: Project, global: bool = false) =
        let
            tv = self.pat.preevalLet(project, global)
        if self.default.isSome:
            discard self.default.get.preeval(project, global)
        if self.typ.isSome:
            let
                t = self.typ.get.eval(project, global)
            self.typ.get.typ = t.typ
            project.env.coerce(tv == t)
    proc preevalVar(self: Ident, project: Project, global: bool = false): Type =
        let
            tv = Type.Var(project.env)
            symbol = Symbol.Var(self, tv, global)
        project.addErr project.env.addSymbol(symbol)
        tv
    proc preevalVar(self: Pattern, project: Project, global: bool = false): Type =
        result = case self.kind
        of PatternKind.Literal:
            self.litval.typ
        of PatternKind.Ident:
            self.ident.preevalVar(project, global)
        of PatternKind.Tuple:
            # TODO: tag
            self.patterns.mapIt(it.preevalVar(project, global)).foldl(Type.Pair(a, b))
        of PatternKind.Record:
            # TODO: tag
            var members = initTable[string, Type]()
            for (k, v) in self.members:
                discard k.preevalVar(project, global)
                members[k.name] = v.preevalVar(project, global)
            Type.Record(members)
        self.typ = result
    proc preevalVar(self: IdentDef, project: Project, global: bool = false) =
        let
            tv = self.pat.preevalVar(project, global)
        if self.default.isSome:
            discard self.default.get.preeval(project, global)
        if self.typ.isSome:
            let
                t = self.typ.get.eval(project, global)
            self.typ.get.typ = t.typ
            project.env.coerce(tv == t)
    # proc preevalConst(self: Ident, project: Project, global: bool = false): Type =
    #     let
    #         tv = Type.Var(project.env)
    #         symbol = Symbol.Var(self, tv, global)
    #     project.addErr project.env.addSymbol(symbol)
    #     tv
    # proc preevalConst(self: Pattern, project: Project, global: bool = false): Type =
    #     result = case self.kind
    #     of PatternKind.Literal:
    #         self.litval.typ
    #     of PatternKind.Ident:
    #         self.ident.preevalConst(project, global)
    #     of PatternKind.Tuple:
    #         # TODO: tag
    #         self.patterns.mapIt(it.preevalConst(project, global)).foldl(Type.Pair(a, b))
    #     of PatternKind.Record:
    #         # TODO: tag
    #         var members = initTable[string, Type]()
    #         for (k, v) in self.members:
    #             discard k.preevalConst(project, global)
    #             members[k.name] = v.preevalConst(project, global)
    #         Type.Record(members)
    #     self.typ = result
    proc preevalConst(self: IdentDef, project: Project, global: bool = false) =
        # let
        #     tv = self.pat.preevalConst(project, global)
        # if self.default.isSome:
        #     let
        #         t = self.default.get.eval(project, global)
        #     project.env.coerce(t.typ <= tv)
        # if self.typ.isSome:
        #     let
        #         t = self.typ.get.eval(project, global)
        #     self.typ.get.typ = t.typ
        #     project.env.coerce(tv == t)
        debug self.default.get.eval(project, global)
        assert false, "notimplemented"
    proc preeval(self: GenTypeDef, project: Project, global: bool = false) =
        if self.ub.isSome:
            let ub = self.ub.get.eval(project, global)
            project.env.coerce(self.ident.typ.gt.ub == ub)
        if self.typ.isSome:
            let typ = self.typ.get.eval(project, global)
            project.env.coerce(self.ident.typ.gt.typ == typ)
    proc preeval(self: TypeDef, project: Project, global: bool = false) =
        for e in self.params:
            e.preeval(project, global)
        let
            typ = self.typ.eval(project, global)
        project.env.coerce(self.ident.typ.symbol.get.val.rety == typ)
    proc preeval(self: Pattern, project: Project, global: bool = false): Type =
        result = case self.kind
        of PatternKind.Literal:
            self.litval.typ
        of PatternKind.Ident:
            self.ident.preeval(project, global)
        of PatternKind.Tuple:
            # TODO: tag
            self.patterns.mapIt(it.preeval(project, global)).foldl(Type.Pair(a, b))
        of PatternKind.Record:
            # TODO: tag
            var members = initTable[string, Type]()
            for (k, v) in self.members:
                discard k.preeval(project, global)
                members[k.name] = v.preeval(project, global)
            Type.Record(members)
        self.typ = result
    proc preevalParam(self: Ident, project: Project, global: bool = false): Type =
        let
            tv = Type.Var(project.env)
            symbol = Symbol.Param(self, tv, global)
        project.addErr project.env.addSymbol(symbol)
        tv
    proc preevalParam(self: Pattern, project: Project, global: bool = false): Type =
        result = case self.kind
        of PatternKind.Literal:
            self.litval.typ
        of PatternKind.Ident:
            self.ident.preevalParam(project, global)
        of PatternKind.Tuple:
            # TODO: tag
            self.patterns.mapIt(it.preevalParam(project, global)).foldl(Type.Pair(a, b))
        of PatternKind.Record:
            # TODO: tag
            var members = initTable[string, Type]()
            for (k, v) in self.members:
                discard k.preevalParam(project, global)
                members[k.name] = v.preevalParam(project, global)
            Type.Record(members)
        self.typ = result
    proc preevalParam(self: IdentDef, project: Project, global: bool = false): Type =
        let
            tv = self.pat.preevalParam(project, global)
        if self.default.isSome:
            discard self.default.get.preeval(project, global)
        assert self.typ.isSome
        let
            t = self.typ.get.eval(project, global)
        self.typ.get.typ = t.typ
        project.env.coerce(tv == t)
        t
    proc preeval(self: FunctionSignature, project: Project, global: bool = false) =
        let
            fnty = self.ident.typ
            paramty = self.params.mapIt(it.preevalParam(project, global))
            rety = self.rety.eval(project, global)
        project.env.coerce(fnty.rety == Type.Arrow(paramty, rety))
    proc preeval(self: Function, project: Project, global: bool = false) =
        project.env.enter self.body.scope:
            self.signature.preeval(project, global)
            discard self.body.preeval(project, global)
    result = case self.kind
    of ExpressionKind.Literal:
        self.litval.typ
    of ExpressionKind.Ident:
        self.ident.preeval(project, global)
    of ExpressionKind.Call:
        discard self.callee.preeval(project, global)
        for e in self.args:
            discard e.preeval(project, global)
        Type.Var(project.env)
    of ExpressionKind.Apply:
        discard self.callee.preeval(project, global)
        for e in self.args:
            discard e.preeval(project, global)
        if self.callee.typ.symbol.isSome and self.callee.typ.symbol.get.kind == SymbolKind.Type:
            discard
        else:
            let
                getId = newIdent("[]", self.loc)
                getExp = Expression.Id(getId, self.loc)
                scope = self.scope
            discard getExp.preeval(project, global)
            self[] = Expression.Call(getExp, @[self.callee] & self.args, self.loc)[]
            self.scope = scope
        Type.Var(project.env)
    of ExpressionKind.If:
        discard self.cond.preeval(project, global)
        project.env.enter self.then.scope:
            discard self.then.preeval(project, global)
        project.env.enter self.els.scope:
            discard self.els.preeval(project, global)
        Type.Var(project.env)
    of ExpressionKind.Case:
        # TODO: Pattern
        Type.Var(project.env)
    of ExpressionKind.Pair:
        Type.Pair(self.first.preeval(project, global), self.second.preeval(project, global))
    of ExpressionKind.Array:
        Type.Array(self.elements.len.uint, Type.Var(project.env))
    of ExpressionKind.Record:
        var
            members = initTable[string, Type]()
        for k, v in self.members.pairs:
            let
                tv = v.preeval(project, global)
            k.typ = tv
            members[k.name] = tv
        Type.Record(members)
    of ExpressionKind.ObjCons:
        discard self.obj.preeval(project)
        let
            obj = self.obj.typ.symbol.get.val
            implicits = self.implicits.mapIt(it.eval(project))
        self.obj.typ = obj.typ
        for i, e in implicits.pairs:
            self.implicits[i].typ = e.typ
        for k, v in self.members.pairs:
            discard k.preeval(project, global)
            discard v.preeval(project, global)
        obj.inst(implicits)
    of ExpressionKind.Ref:
        Type.Ptr(self.to.preeval(project, global))
    of ExpressionKind.Import:
        # TODO:
        Type.Unit
    of ExpressionKind.LetSection:
        for e in self.iddefs:
            e.preevalLet(project, global)
        Type.Unit
    of ExpressionKind.VarSection:
        for e in self.iddefs:
            e.preevalVar(project, global)
        Type.Unit
    of ExpressionKind.ConstSection:
        for e in self.iddefs:
            e.preevalConst(project, global)
        Type.Unit
    of ExpressionKind.TypeSection:
        project.env.enter self.scope:
            self.typedef.preeval(project, global)
        Type.Unit
    of ExpressionKind.Assign:
        discard self.assign_lval.preeval(project, global)
        discard self.assign_val.preeval(project, global)
        Type.Unit
    of ExpressionKind.Funcdef:
        self.fn.preeval(project, global)
        Type.Unit
    of ExpressionKind.ImportLL:
        # TODO:
        Type.Unit
    of ExpressionKind.Loop:
        self.`block`.preeval(project, global)
    of ExpressionKind.Discard:
        self.`block`.preeval(project, global)
    of ExpressionKind.Seq:
        for e in self.expressions:
            discard e.preeval(project, global)
        if self.expressions.len == 0:
            Type.Unit
        else:
            self.expressions[^1].typ
    of ExpressionKind.Typeof:
        Type.Singleton(self.`typeof`.preeval(project, global))
    of ExpressionKind.Malloc:
        discard self.msize.preeval(project, global)
        Type.Ptr(self.mtype.eval(project, global))
    of ExpressionKind.Realloc:
        discard self.msize.preeval(project, global)
        self.rptr.preeval(project, global)
    of ExpressionKind.PtrSet:
        discard self.`ptr`.preeval(project, global)
        discard self.index.preeval(project, global)
        discard self.val.preeval(project, global)
        Type.Var(project.env)
    of ExpressionKind.PtrGet:
        discard self.`ptr`.preeval(project, global)
        discard self.index.preeval(project, global)
        Type.Var(project.env)
    self.typ = result
proc posteval*(self: Expression, project: Project): Type =
    case self.kind
    of ExpressionKind.Literal:
        self.litval.eval(project)
    of ExpressionKind.Ident:
        self.typ.symbol.get.val.inst(project.env)
    of ExpressionKind.Call:
        Type.Unit
    of ExpressionKind.Apply:
        Type.Unit
    of ExpressionKind.If:
        Type.Unit
    of ExpressionKind.Case:
        Type.Unit
    of ExpressionKind.Pair:
        Type.Pair(self.first.posteval(project), self.second.posteval(project))
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
    of ExpressionKind.ConstSection:
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
        self.typ.base
    of ExpressionKind.Malloc:
        Type.Unit
    of ExpressionKind.Realloc:
        Type.Unit
    of ExpressionKind.PtrSet:
        Type.Unit
    of ExpressionKind.PtrGet:
        Type.Unit

proc eval*(self: Expression, project: Project, global: bool = false): Type =
    project.env.init_cons:
        self.predeclare(project, global)
        discard self.preeval(project, global)
        project.env.resolveEq
        self.infer(project, global)
        project.env.resolve
        discard self.check(project)
    self.posteval(project)
