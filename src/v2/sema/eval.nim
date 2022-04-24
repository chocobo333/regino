
import options
import sequtils
import sugar
import sets
import tables

import ../il
import ../typeenv
import ../errors

import ../orders
import ../utils
import ../lineinfos

import infer as _


proc eval*(self: Program, env: TypeEnv): Value
proc eval*(self: Suite, env: TypeEnv): Value
proc eval*(self: Statement, env: TypeEnv, global: bool = false): Value
proc eval*(self: Expression, env: TypeEnv, global: bool = false): Value
proc eval*(self: TypeExpression, env: TypeEnv, global: bool = false): Value
proc check(self: Expression, env: TypeEnv)
proc infer*(self: Literal): Value =
    ## that retrurns literal's type merely
    self.typ
proc infer*(self: Statement, env: TypeEnv, global: bool = false): Value
proc infer*(self: Suite, env: TypeEnv): Value =
    if self.stmts.len == 0:
        # Suite which has no statements has () type
        # because it is the same as Suite which has only mere discard statement
        return Value.Unit
    env.enter(self.scope):
        for s in self.stmts[0..^2]:
            discard s.infer(env)
            # env.coerce(s.infer(env) == Value.Unit, TypeError.Discard(s))
        result = self.stmts[^1].infer(env)
    env.resolveRelationsPartially()
proc infer*(self: ElifBranch, env: TypeEnv, global: bool = false): Value =
    let
        cond = self.cond.infer(env, global)
    env.coerce(cond == Value.Bool)
    self.suite.infer(env)
proc infer*(self: Ident, env: TypeEnv, global: bool = false): Value =
    let
        syms = env.lookupId(self.name)
    result = case syms.len
    of 0:
        Value.Var(env)
    of 1:
        syms[0].typ.inst(env)
    else:
        Value.Intersection(syms.mapIt(it.typ.inst(env)))
    self.typ = result
proc infer*(self: Expression, env: TypeEnv, global: bool = false): Value =
    result = case self.kind
    of ExpressionKind.Literal:
        self.litval.infer
    of ExpressionKind.Ident:
        self.ident.infer(env, global)
    of ExpressionKind.Tuple:
        self.exprs.mapIt(it.infer(env, global)).foldl(Value.Pair(a, b))
    of ExpressionKind.Array:
        let
            tv = Value.Var(env)
            elements = self.exprs.mapIt(it.infer(env, global))
        debug self.loc
        debug elements
        for t in elements:
            env.coerce(t <= tv)
        Value.Array(tv)
        # Value.Unit
    of ExpressionKind.Record:
        Value.Record(self.members.mapIt((it[0], it[1].infer(env, global))).toTable)
    of ExpressionKind.If:
        let
            conds = self.elifs.mapIt(it.cond.infer(env, global))
            thens = self.elifs.mapIt(it.suite.infer(env))
            elset = self.elseb.map(it => it.infer(env))
            tv = Value.Var(env)
        for cond in conds:
            env.coerce(cond == Value.Bool)
        for t in thens:
            env.coerce(t <= tv)
        if elset.isSome:
            env.coerce(elset.get <= tv)
        tv
    of ExpressionKind.When:
        Value.Unit
    of ExpressionKind.Case:
        Value.Unit
    of ExpressionKind.Call, ExpressionKind.Command:
        let
            tv = Value.Var(env)
            args = self.args.mapIt(it.infer(env, global))
            callee = self.callee.infer(env, global)
        env.coerce(callee <= Value.Arrow(args, tv))
        env.coerce(Value.Arrow(args.mapIt(Value.Unit), tv) <= callee.dual) # i dont know whether this is correct.
        tv
    of ExpressionKind.Dot:
        let
            tv = Value.Var(env)
            args = @[self.lhs.infer(env, global)]
            callee = self.rhs.infer(env, global)
        env.coerce(callee <= Value.Arrow(args, tv))
        env.coerce(Value.Arrow(@[Value.Unit], tv) <= callee.dual) # i dont know whether this is correct.
        tv
    of ExpressionKind.Bracket:
        Value.Unit
    of ExpressionKind.Binary:
        let
            tv = Value.Var(env)
            lhs = self.lhs.infer(env, global)
            rhs = self.rhs.infer(env, global)
            op = self.op.infer(env, global)
        env.coerce(op <= Value.Arrow(@[lhs, rhs], tv))
        env.coerce(Value.Arrow(@[Value.Unit, Value.Unit], tv) <= op.dual) # i dont know whether this is correct.
        tv
    of ExpressionKind.Prefix, ExpressionKind.Postfix:
        let
            tv = Value.Var(env)
            exp = self.expression.infer(env, global)
            op = self.op.infer(env, global)
        env.coerce(op <= Value.Arrow(@[exp], tv))
        env.coerce(Value.Arrow(@[Value.Unit], tv) <= op.dual)
        tv
    of ExpressionKind.Block:
        self.`block`.infer(env)
    of ExpressionKind.Lambda:
        Value.Unit
    of ExpressionKind.Malloc:
        env.coerce(self.msize.infer(env, global) == Value.Integer)
        Value.Ptr(self.mtype.eval(env, global))
    of ExpressionKind.Typeof:
        Value.Singleton(self.`typeof`.infer(env, global))
    of ExpressionKind.Ref:
        Value.Unit
    of ExpressionKind.FnType:
        Value.Unit
    of ExpressionKind.Fail:
        Value.Bottom
    self.typ = result
proc infer*(self: Metadata, env: TypeEnv, global: bool = false): Value =
    for param in self.params:
        discard param.infer(env)
    Value.Unit
proc infer*(self: Pattern, env: TypeEnv, global: bool = false, asign: bool = false): Value =
    result = case self.kind
    of PatternKind.Literal:
        self.litval.infer
    of PatternKind.Ident:
        # TODO: index
        if asign:
            self.ident.infer(env, global)
        else:
            let res = Value.Var(env)
            self.ident.typ = res
            res
    of PatternKind.Tuple:
        self.patterns.mapIt(it.infer(env, global, asign)).foldl(Value.Pair(a, b))
    of PatternKind.Record:
        # TODO:
        Value.Unit
    of PatternKind.UnderScore:
        Value.Var(env)
    self.typ = result
proc infer(self: TypeExpression, env: TypeEnv): Value =
    case self.kind
    of TypeExpressionKind.Object:
        # TODO: ituka jissousuru
        assert false, "notimplimented"
        Value.Unit
    of TypeExpressionKind.Sum:
        # TODO: Add constructor
        let cons = self.sum.constructors.mapIt((
            it.id,
            case it.kind
            of SumConstructorKind.NoField:
                Value.Unit
            of SumConstructorKind.UnnamedField:
                Expression.Tuple(it.types).infer(env)
            of SumConstructorKind.NamedField:
                Expression.Record(it.fields).infer(env)
        ))
        Value.Sum(cons.toTable)
    of TypeExpressionKind.Distinct:
        Value.Distinct(self.base.infer(env))
    of TypeExpressionKind.Trait:
        assert false, "notimplimented"
        Value.Unit
    of TypeExpressionKind.Expression:
        self.expression.infer(env)
proc addPatL(env: TypeEnv, impl: IdentDef, pat: Pattern = impl.pat, global: bool = false) =
    ## register identifier on typeenv
    case pat.kind
    of PatternKind.Literal:
        discard
    of PatternKind.Ident:
        # TODO: index
        let sym = Symbol.Let(pat.ident, pat.typ, impl, global)
        env.addIdent(sym)
    of PatternKind.Tuple:
        for pat in pat.patterns:
            env.addPatL(impl, pat, global)
    of PatternKind.Record:
        discard
    of PatternKind.UnderScore:
        discard
proc addPatV(env: TypeEnv, impl: IdentDef, pat: Pattern = impl.pat, global: bool = false) =
    ## register identifier on typeenv
    case pat.kind
    of PatternKind.Literal:
        discard
    of PatternKind.Ident:
        # TODO: index
        let sym = Symbol.Var(pat.ident, pat.typ, impl, global)
        env.addIdent(sym)
    of PatternKind.Tuple:
        for pat in pat.patterns:
            env.addPatV(impl, pat, global)
    of PatternKind.Record:
        discard
    of PatternKind.UnderScore:
        discard
proc PairToSeq(pair: Value): seq[Value] =
    if pair.kind == ValueKind.Pair:
        @[pair.first] & pair.second.PairToSeq
    else:
        @[pair]
proc infer(self: GenTypeDef, env: TypeEnv, global: bool = false): GenericType =
    # infer and add symbol
    let
        id = self.id
        ub = self.ub.map(it => it.eval(env, global)).get(Value.Unit)
        genty = newGenericType(id, ub, ub.typ)
        typ = Value.Gen(genty)
        sym = Symbol.GenParam(id, typ, self)
    env.addIdent(sym)
    genty
proc addParam(env: TypeEnv, impl: IdentDef, typ: Value, pat: Pattern = impl.pat, global: bool = false) =
        ## register identifier on typeenv
        case pat.kind
        of PatternKind.Literal:
            discard
        of PatternKind.Ident:
            # TODO: index
            let sym = Symbol.Param(pat.ident, typ, impl, global)
            pat.ident.typ = typ
            env.addIdent(sym)
        of PatternKind.Tuple:
            assert typ.kind == ValueKind.Pair
            for (pat, typ) in pat.patterns.zip(typ.PairToSeq):
                env.addParam(impl, typ, pat, global)
        of PatternKind.Record:
            discard
        of PatternKind.UnderScore:
            discard
        pat.typ = typ
proc addFunc(env: TypeEnv, fn: Function, global: bool = false) =
    var
        sym: Symbol
        rety: Value
        fnty: Value
    env.enter(fn.param.scope):
        let
            implicit = fn.param.implicit.mapIt(it.infer(env, global))
            paramty = fn.param.params.mapIt(block:
                # TODO: pattern like a, b: int
                assert it.typ.isSome
                let paramty = it.typ.get.eval(env, global)
                env.addParam(it, paramty)
                paramty
            )
        rety = fn.param.rety.map(it => it.eval(env, global)).get(Value.Unit)
        fnty = Value.Pi(implicit, paramty, rety)
        sym = Symbol.Func(fn.id, fnty, global)
    env.addIdent(sym)
    fn.id.typ = fnty
    if fn.metadata.isSome:
        if fn.metadata.get.kind == MetadataKind.Subtype:
            assert fn.param.params.len == 1, "converter must take only one argument."
            env.addTypeRelation(sym.typ.params[0], rety, fn.id)
proc infer(fn: Function, env: TypeEnv, global: bool = false) =
    let
        sym = fn.id.typ.symbol.get
        rety = fn.id.typ.rety
    env.enter(fn.param.scope):
        if fn.suite.isSome:
            let infered = fn.suite.get.infer(env)
            env.coerce(infered <= rety)

proc infer*(self: Statement, env: TypeEnv, global: bool = false): Value =
    result = case self.kind
    of StatementKind.For:
        Value.Unit
    of StatementKind.While:
        let branch = self.branch.infer(env)
        env.coerce(branch == Value.Unit)
        Value.Unit
    of StatementKind.Loop:
        Value.Unit
    of StatementKind.LetSection:
        for iddef in self.iddefs:
            let
                pat = iddef.pat
                typ = iddef.typ
                default = iddef.default
                paty = pat.infer(env, global)
            if default.isSome:
                let t = default.get.infer(env, global)
                env.coerce(t <= paty)
            if typ.isSome:
                let
                    typ = typ.get
                    tv = typ.infer(env, global)
                typ.check(env)
                let t = typ.eval(env, global)
                env.coerce(t == paty)
            env.addPatL(iddef, global=global)
        Value.Unit
    of StatementKind.VarSection:
        for iddef in self.iddefs:
            let
                pat = iddef.pat
                typ = iddef.typ
                default = iddef.default
                paty = pat.infer(env, global)
            if default.isSome:
                let t = default.get.infer(env, global)
                env.coerce(t <= paty)
            if typ.isSome:
                let
                    typ = typ.get
                    tv = typ.infer(env, global)
                typ.check(env)
                let t = typ.eval(env, global)
                env.coerce(t == paty)
            env.addPatV(iddef, global=global)
        Value.Unit
    of StatementKind.ConstSection:
        Value.Unit
    of StatementKind.TypeSection:
        for typedef in self.typedefs:
            let
                id = typedef.id
                params = typedef.params
                typ = typedef.typ
            if params.isNone:
                # TODO: infer and check
                let _ = typ.infer(env)
                # typ.check(env)
                let
                    typ = typ.eval(env, global)
                    sym = Symbol.Typ(id, typ, typedef, global)
                env.addIdent(sym)
            else:
                let scope = newScope(env.scope)
                var sym: Symbol
                env.enter scope:
                    let implicit = params.get.mapIt(it.infer(env, global))
                    let
                        _ = typ.infer(env)
                        typ = typ.eval(env, global)
                    sym = Symbol.Typ(id, Value.Cons(implicit, typ), typedef, global)
                env.addIdent(sym)
        Value.Unit
    of StatementKind.Asign:
        let
            paty = self.pat.infer(env, global, true)
            val = self.val.infer(env, global)
        env.coerce(val <= paty)
        Value.Unit
    of StatementKind.Funcdef:
        env.addFunc(self.fn, global)
        self.fn.infer(env, global)
        Value.Unit
    of StatementKind.Meta:
        discard self.meta.infer(env)
        Value.Unit
    of StatementKind.Discard:
        if self.`discard`.isSome:
            discard self.`discard`.get.infer(env, global)
        Value.Unit
    of StatementKind.Comments:
        Value.Unit
    of StatementKind.Expression:
        self.expression.infer(env, global)
    of StatementKind.Fail:
        Value.Unit
proc infer*(self: Program, env: TypeEnv): Value =
    ## Entry point of type inference algorithm
    if self.stmts.len == 0:
        return Value.Unit
    # for s in self.stmts.filterIt(it.kind == StatementKind.Funcdef):
    # for s in self.stmts.filterIt(it.kind == StatementKind.Funcdef):
    #     env.addFunc(s.fn, true)
    for s in self.stmts[0..^2]:
        discard s.infer(env)
        # env.coerce(s.infer(env) == Value.Unit, TypeError.Discard(s))
    result = self.stmts[^1].infer(env)
    debug env.constraints
    env.resolveRelations()
    debug env.constraints
    debug env.tvs
    for e in env.tvs:
        debug e.tv.lb
        debug e.tv.ub

proc check(self: Suite, env: TypeEnv)
proc check(self: Ident, env: TypeEnv) =
    if self.typ.symbol.isNone:
        if self.typ.kind == ValueKind.Intersection:
            env.errs.add TypeError.Undeciable(self.loc)
        else:
            env.errs.add TypeError.Undefined(self, self.loc)
    else:
        self.typ.symbol.get.use.add self.loc
proc check(self: Pattern, env: TypeEnv) =
    case self.kind:
    of PatternKind.Literal:
        discard
    of PatternKind.Ident:
        discard
    of PatternKind.Tuple:
        discard
    of PatternKind.Record:
        discard
    of PatternKind.UnderScore:
        discard
proc check(self: Expression, env: TypeEnv) =
    setTypeEnv(env)
    # TODO: invalid
    # if self.typ.kind == ValueKind.Link:
    #     env.bindtv(self.typ, self.typ.to)
    if self.typ.kind == ValueKind.Var:
        if self.kind == ExpressionKind.Ident and self.typ.symbol.isNone:
            # TypeError.Undefined(self.ident, self.loc)
            discard
        else:
            env.errs.add TypeError.Undeciable(self.loc)
    case self.kind
    of ExpressionKind.Literal:
        discard
    of ExpressionKind.Ident:
        self.ident.check(env)
    of ExpressionKind.Tuple:
        for e in self.exprs:
            e.check(env)
    of ExpressionKind.Array:
        for e in self.exprs:
            e.check(env)
    of ExpressionKind.Record:
        discard
    of ExpressionKind.If:
        for e in self.elifs:
            let
                cond = e.cond
                suite = e.suite
            cond.check(env)
            suite.check(env)
            assert cond.typ == Value.Bool
            assert suite.typ <= self.typ
        if self.elseb.isSome:
            let suite = self.elseb.get
            suite.check(env)
            assert suite.typ <= self.typ
    of ExpressionKind.When:
        discard
    of ExpressionKind.Case:
        discard
    of ExpressionKind.Call, ExpressionKind.Command:
        # TODO: insert converter
        self.callee.check(env)
        for arg in self.args:
            arg.check(env)
    of ExpressionKind.Dot:
        discard
    of ExpressionKind.Bracket:
        discard
    of ExpressionKind.Binary:
        discard
    of ExpressionKind.Prefix, ExpressionKind.Postfix:
        discard
    of ExpressionKind.Block:
        discard
    of ExpressionKind.Lambda:
        discard
    of ExpressionKind.Malloc:
        discard
    of ExpressionKind.Typeof:
        discard
    of ExpressionKind.Ref:
        discard
    of ExpressionKind.FnType:
        discard
    of ExpressionKind.Fail:
        env.errs.add TypeError.SomethingWrong(self.loc)
proc check(self: Statement, env: TypeEnv) =
    setTypeEnv(env)
    case self.kind
    of StatementKind.For:
        discard
    of StatementKind.While:
        discard
    of StatementKind.Loop:
        discard
    of StatementKind.LetSection:
        for iddef in self.iddefs:
            # TODO: implement check(Pattern)
            # iddef.pat.check(env)
            # TODO: Shoud I check TypeExpression?
            # typ should be infered, checked and evaled.
            if iddef.typ.isSome:
                iddef.typ.get.check(env)
            if iddef.default.isSome:
                iddef.default.get.check(env)
    of StatementKind.VarSection:
        discard
    of StatementKind.ConstSection:
        discard
    of StatementKind.TypeSection:
        discard
    of StatementKind.Asign:
        self.pat.check(env)
        self.val.check(env)
        # case self.pat.ident
        for ident in collectIdent(self.pat):
            if ident.typ.symbol.isSome:
                if ident.typ.symbol.get.kind != SymbolKind.Var:
                    env.errs.add TypeError.Letasign(ident, ident.loc)
        case self.op.name
        of "=":
            if self.val.typ <= self.pat.typ:
                if not (self.pat.typ <= self.val.typ):
                    let
                        conv = env.lookupConverter(self.val.typ, self.pat.typ)
                    debug conv
                    if conv.isSome:
                        let
                            ident = conv.get
                            callee = Expression.Id(ident, ident.loc)
                            val = Expression.Call(callee, @[self.val])
                        val.typ = self.pat.typ
                        self.val = val
                    else:
                        assert false, "t1 <= t2 but t1 is not convertable to t2"
            else:
                env.errs.add TypeError.Unasignable(self.pat, self.val, self.loc)
        else:
            # TODO:
            discard
    of StatementKind.Funcdef:
        discard
    of StatementKind.Meta:
        discard
    of StatementKind.Discard:
        if self.`discard`.isSome:
            self.`discard`.get.check(env)
    of StatementKind.Comments:
        discard
    of StatementKind.Expression:
        self.expression.check(env)
    of StatementKind.Fail:
        discard
proc check(self: Suite, env: TypeEnv) =
    env.enter(self.scope):
        if self.isFailed:
            # TODO: Suite have to have parameter `loc`
            env.errs.add TypeError.NoSuite(self.loc)
            return
        for s in self.stmts[0..^2]:
            s.check(env)
            if s.typ != Value.Unit:
                env.errs.add TypeError.Discard(s)
        self.stmts[^1].check(env)
proc check*(self: Program, env: TypeEnv) =
    if self.stmts.len == 0:
        return
    for s in self.stmts[0..^2]:
        s.check(env)
        if s.typ != Value.Unit:
            env.errs.add TypeError.Discard(s)
    self.stmts[^1].check(env)

proc eval*(self: Literal): Value =
    Value.literal(self)
proc eval*(self: TypeExpression, env: TypeEnv, global: bool = false): Value =
    case self.kind
    of TypeExpressionKind.Object:
        Value.Unit
    of TypeExpressionKind.Sum:
        let cons = self.sum.constructors.mapIt((
            it.id,
            case it.kind
            of SumConstructorKind.NoField:
                Value.Unit
            of SumConstructorKind.UnnamedField:
                Expression.Tuple(it.types).eval(env)
            of SumConstructorKind.NamedField:
                Expression.Record(it.fields).eval(env)
        ))
        Value.Sum(cons.toTable)
    of TypeExpressionKind.Distinct:
        Value.Distinct(self.base.eval(env, global))
    of TypeExpressionKind.Trait:
        Value.Unit
    of TypeExpressionKind.Expression:
        self.expression.eval(env, global)
proc eval*(self: Expression, env: TypeEnv, global: bool = false): Value =
    discard self.infer(env, global)
    case self.kind
    of ExpressionKind.Literal:
        self.litval.eval
    of ExpressionKind.Ident:
        if self.typ.symbol.isSome:
            return self.typ.symbol.get.val
        let
            syms = env.lookupId(self.ident.name)
        case syms.len
        of 0:
            Value.Var(env)
        of 1:
            syms[0].val
        else:
            Value.Intersection(syms.mapIt(it.val))
    of ExpressionKind.Tuple:
        self.exprs.mapIt(it.eval(env, global)).foldl(Value.Pair(a, b))
    of ExpressionKind.Array:
        Value.Array(self.exprs.mapIt(it.eval(env)))
    of ExpressionKind.Record:
        Value.Record(self.members.mapIt((it[0], it[1].eval(env, global))).toTable)
    of ExpressionKind.If:
        var ret = Value.Bottom
        for `elif` in self.elifs & self.elseb.map(it => @[newElif(Expression.literal(true), it)]).get(@[]):
            let
                cond = `elif`.cond
                suite = `elif`.suite
            if cond.eval(env).litval.boolval == false:
                continue
            ret = suite.eval(env)
            break
        ret
    of ExpressionKind.When:
        Value.Unit
    of ExpressionKind.Case:
        Value.Unit
    of ExpressionKind.Call:
        Value.Unit
    of ExpressionKind.Command:
        Value.Unit
    of ExpressionKind.Dot:
        Value.Unit
    of ExpressionKind.Bracket:
        let
            val = self.callee.eval(env, global)
        case val.kind:
        of ValueKind.Pi, ValueKind.Cons:
            let subs = zip(val.implicit, self.args.mapIt(it.eval(env))).toTable
            val.inst(env, subs)
        else:
            # TODO: ituka
            Value.Unit
    of ExpressionKind.Binary:
        Value.Unit
    of ExpressionKind.Prefix:
        Value.Unit
    of ExpressionKind.Postfix:
        Value.Unit
    of ExpressionKind.Block:
        self.`block`.eval(env)
    of ExpressionKind.Lambda:
        Value.Unit
    of ExpressionKind.Malloc:
        Value.Ptr(self.mtype.eval(env, global))
    of ExpressionKind.Typeof:
        self.`typeof`.infer(env, global)
    of ExpressionKind.Ref:
        Value.Ptr(self.`ref`.eval(env, global))
    of ExpressionKind.FnType:
        Value.Arrow(self.args.mapIt(it.eval(env, global)), self.rety.eval(env, global))
    of ExpressionKind.Fail:
        Value.Bottom
proc asign(self: Pattern, val: Value) =
    # TODO: for others
    case self.kind
    of PatternKind.Literal:
        discard
    of PatternKind.Ident:
        self.ident.typ.symbol.get.val = val
    of PatternKind.Tuple:
        if val.kind == ValueKind.Pair:
            self.patterns[0].asign(val.first)
            Pattern.Tuple(self.patterns[1..^1]).asign(val.second)
        else:
            self.patterns[0].asign(val)
    of PatternKind.Record:
        discard
    of PatternKind.UnderScore:
        discard
proc eval*(self: Statement, env: TypeEnv, global: bool = false): Value =
    case self.kind
    of StatementKind.For:
        Value.Unit
    of StatementKind.While:
        Value.Unit
    of StatementKind.Loop:
        Value.Unit
    of StatementKind.LetSection:
        for e in self.iddefs:
            e.pat.asign(e.default.get.eval(env, global))
        Value.Unit
    of StatementKind.VarSection:
        for e in self.iddefs:
            if e.default.isSome:
                e.pat.asign(e.default.get.eval(env, global))
        Value.Unit
    of StatementKind.ConstSection:
        Value.Unit
    of StatementKind.TypeSection:
        Value.Unit
    of StatementKind.Asign:
        case self.op.name
        of "=":
            self.pat.asign(self.val.eval(env, global))
        else:
            # TODO: like "+="
            discard
        Value.Unit
    of StatementKind.Funcdef:
        Value.Unit
    of StatementKind.Meta:
        Value.Unit
    of StatementKind.Discard:
        if self.`discard`.isSome:
            discard self.`discard`.get.eval(env, global)
        Value.Unit
    of StatementKind.Comments:
        Value.Unit
    of StatementKind.Expression:
        self.expression.eval(env, global)
    of StatementKind.Fail:
        Value.Unit
proc eval*(self: Suite, env: TypeEnv): Value =
    env.enter self.scope:
        for s in self.stmts:
            result = s.eval(env)
proc eval*(self: Program, env: TypeEnv): Value =
    # discard self.infer(env)
    # self.check(env)
    for s in self.stmts:
        result = s.eval(env, true)
