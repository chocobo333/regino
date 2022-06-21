
import options
import sequtils
import sugar
import sets
import tables
import algorithm

import inst
import ../il
import ../typeenv
import ../errors

import ../utils
import ../lineinfos

import coerce
import resolve

import decls


proc check(self: Suite, env: TypeEnv)
proc eval*(self: Program, env: TypeEnv): Value
proc eval*(self: Suite, env: TypeEnv): Value
proc eval*(self: Statement, env: TypeEnv, global: bool = false): Value
proc eval*(self: TypeExpression, env: TypeEnv, ident: Ident, global: bool = false): Value


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
proc resolveLink(self: Value) =
    if self.kind == ValueKind.Link:
        self.to.resolveLink
        let
            symbol = self.symbol
        self[] = self.to[]
        self.symbol = symbol

proc coercion(self: TypeEnv, v1, v2: Value, e: Expression): Expression =
    if v1 == v2:
        return e
    result = if (v1, v2) in self.scope.converters:
        Expression.Call(Expression.Id(self.scope.converters[(v1, v2)]), @[e])
    elif v2.kind == ValueKind.Unit:
        let suite = newSuite(@[
            Statement.Discard(some e),
            Statement.Expr(Expression.literal(Literal.unit))
        ])
        suite.scope = newScope(self.scope)
        Expression.Block(suite)
    else:
        if v2.kind == ValueKind.Distinct:
            self.coercion(v1, v2.base, e)
        elif v1.kind == v2.kind:
            case v1.kind
            of ValueKind.Integer:
                Expression.IntCast(e, v1.bits, v2.bits)
            of ValueKind.Pair:
                let scope = newScope(self.scope)
                var
                    a: Expression
                    b: Expression
                self.enter scope:
                    a = self.coercion(v1.first, v2.first, Expression.Id("a"))
                    b = self.coercion(v1.second, v2.second, Expression.Id("b"))
                let suite = newSuite(
                    @[
                        Statement.LetSection(
                            newIddefSection(@[
                                newIdentdef(
                                    Pattern.Tuple(@[Pattern.Id("a"), Pattern.Id("b")]),
                                    some Expression.Typeof(e),
                                    some e
                                )
                            ], @[])
                        ),
                        Statement.Expr(Expression.Tuple(@[a, b]))
                    ]
                )
                suite.scope = scope
                Expression.Block(suite)
            of ValueKind.Record:
                let scope = newScope(self.scope)
                var
                    pat: Pattern = Pattern.Record(toSeq(v2.members.keys).mapIt((it, Pattern.Id(it.name))))
                    exp: Expression = Expression.Record(@[])
                self.enter scope:
                    for (id, val) in v2.members.pairs:
                        exp.members.add (
                            id,
                            self.coercion(v1.members[id], val, Expression.Id(id.name))
                        )
                let suite = newSuite(
                    @[
                        Statement.LetSection(
                            newIddefSection(@[
                                newIdentdef(
                                    pat,
                                    some Expression.Typeof(e),
                                    some e
                                )
                        ], @[])),
                        Statement.Expr(exp)
                    ]
                )
                suite.scope = scope
                Expression.Block(suite)
            else:
                debug v1
                debug v2
                nil
        else:
            debug v1
            debug v2
            nil
    if result.isNil:
        self.errs.add TypeError.NoCoercion(v1, v2, e.loc)
        result = e
    else:
        result.loc = e.loc
        result.inserted = true
proc coercion(self: TypeEnv, e: Expression, v: Value): Expression =
    setTypeEnv(self)
    if not (e.typ <= v):
        self.errs.add TypeError.NoCoercion(e.typ, v, e.loc)
        return e
    result = e
    if v <= e.typ:
        return
    # assert self.path(e.typ, v).len == 1
    for (s, t) in self.path(e.typ, v).sort(it => it.len)[0]:
        result = self.coercion(s, t, result)
        # result.typ = t
        discard result.infer(self)
        self.resolve # kore de iino ka?
        # result.check(self)
proc check(self: Statement, env: TypeEnv)
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
proc check(self: Function, env: TypeEnv) =
    # TODO: check params
    env.enter self.param.scope:
        if self.param.rety.isSome:
            self.param.rety.get.check(env)
        for iddef in self.param.params:
            # TODO: implement check(Pattern)
            # iddef.pat.check(env)
            # TODO: Shoud I check TypeExpression?
            # typ should be infered, checked and `eval`ed.
            if iddef.typ.isSome:
                iddef.typ.get.check(env)
            assert iddef.default.isNone
        if self.suite.isSome:
            self.suite.get.check(env)
proc check*(self: Expression, env: TypeEnv) {.exportc: "check_e".} =
    setTypeEnv(env)
    self.typ.resolveLink
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
        for i in 0..<self.args.len:
            self.args[i].check(env)
            self.callee.typ.params[i].resolveLink
            self.args[i] = env.coercion(self.args[i], self.callee.typ.params[i])
        if self.callee.typ.symbol.isSome and self.callee.typ.symbol.get.kind == SymbolKind.Func:
            let
                calleety = self.callee.typ
                args = self.args.mapIt(it.typ)
                genty = self.callee.typ.symbol.get.decl_funcdef.param.implicit.mapIt(it.id)
                instances = self.callee.typ.instances
                typdefs = self.callee.typ.symbol.get.decl_funcdef.param.params
            if genty.len > 0:
                # monomorphization
                if calleety notin calleety.symbol.get.instances:
                    let
                        inst = self.callee.typ.symbol.get.decl_funcdef.implInst
                        scope = env.scope
                    env.scope = inst.param.scope
                    for i in 0..<genty.len:
                        let
                            id = genty[i]
                            typ = instances[i]
                            typedef = newTypedef(
                                id,
                                none(seq[GenTypeDef]),
                                TypeExpression.Expr(Expression.Id($typ))
                            )
                            sym = Symbol.Typ(id, typ, typedef, false)
                        env.addIdent(sym)
                    env.scope = scope
                    let fn = Statement.Funcdef(inst)
                    discard fn.infer(env, false)
                    env.resolve()
                    fn.check(env) # must be succeded
                    calleety.symbol.get.instances[calleety] = Impl(instance: some(inst))
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
    of ExpressionKind.IntCast:
        self.int_exp.check(env)
    of ExpressionKind.Fail:
        env.errs.add TypeError.SomethingWrong(self.loc)
proc check(self: IdentDefSection, env: TypeEnv) =
    for iddef in self.iddefs:
        # TODO: implement check(Pattern)
        # iddef.pat.check(env)
        # TODO: Shoud I check TypeExpression?
        # typ should be infered, checked and `eval`ed.
        if iddef.typ.isSome:
            iddef.typ.get.check(env)
        if iddef.default.isSome:
            iddef.default.get.check(env)
            iddef.default = some env.coercion(iddef.default.get, iddef.pat.typ)
proc check(self: Statement, env: TypeEnv) =
    setTypeEnv(env)
    case self.kind
    of StatementKind.Import:
        # TODO:
        discard
    of StatementKind.For:
        discard
    of StatementKind.While:
        discard
    of StatementKind.Loop:
        discard
    of StatementKind.LetSection:
        self.iddefSection.check(env)
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
        self.fn.check(env)
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
proc eval*(self: TypeExpression, env: TypeEnv, ident: Ident, global: bool = false): Value {.exportc: "eval_te".} =
    result = case self.kind
    of TypeExpressionKind.Object:
        let rec = Value.Record(self.members.mapIt((it[0], it[1].eval(env, it[0]))).toTable)
        Value.Distinct(ident, rec)
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
        Value.Distinct(ident, Value.Sum(cons.toTable))
    of TypeExpressionKind.Distinct:
        Value.Distinct(ident, self.base.eval(env, ident, global))
    of TypeExpressionKind.Trait:
        Value.Unit
    of TypeExpressionKind.Expression:
        self.expression.eval(env, global)
    if self.isRef:
        result = Value.Ptr(result)
proc eval*(self: Expression, env: TypeEnv, global: bool = false): Value {.exportc: "eval_e".} =
    discard self.infer(env, global)
    case self.kind
    of ExpressionKind.Literal:
        self.litval.eval
    of ExpressionKind.Ident:
        if self.typ.symbol.isSome:
            return self.typ.symbol.get.val.inst(env)
        let
            syms = env.lookupId(self.ident.name)
        case syms.len
        of 0:
            Value.Var(env)
        of 1:
            syms[0].val.inst(env)
        else:
            Value.Select(syms.mapIt(it.val.inst(env)))
    of ExpressionKind.Tuple:
        case self.exprs.len
        of 0:
            Value.Unit
        of 1:
            Value.Pair(self.exprs[0].eval(env, global), Value.Unit)
        else:
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
        of ValueKind.Pi:
            for (e, arg) in val.instances.zip(self.args):
                if e.kind == ValueKind.Var:
                    env.bindtv(e, arg.eval(env))
            val
        of ValueKind.Family:
            for (e, arg) in val.instances.zip(self.args):
                if e.kind == ValueKind.Var:
                    env.bindtv(e, arg.eval(env))
            val.rety
        else:
            # TODO: ituka
            Value.Cons(val, self.args.mapIt(it.eval(env)))
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
        self.`typeof`.typ
    of ExpressionKind.Ref:
        Value.Ptr(self.`ref`.eval(env, global))
    of ExpressionKind.FnType:
        Value.Arrow(self.args.mapIt(it.eval(env, global)), self.rety.eval(env, global))
    of ExpressionKind.IntCast:
        # TODO:
        self.int_exp.eval(env, global)
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
proc eval*(self: IdentDefSection, env: TypeEnv, global: bool = false) =
    for e in self.iddefs:
        e.pat.asign(e.default.get.eval(env, global))
proc eval*(self: Statement, env: TypeEnv, global: bool = false): Value =
    case self.kind
    of StatementKind.Import:
        # TODO:
        Value.Unit
    of StatementKind.For:
        Value.Unit
    of StatementKind.While:
        Value.Unit
    of StatementKind.Loop:
        Value.Unit
    of StatementKind.LetSection:
        self.iddefSection.eval(env, global)
        # for e in self.iddefs:
        #     e.pat.asign(e.default.get.eval(env, global))
        Value.Unit
    of StatementKind.VarSection:
        self.iddefSection.eval(env, global)
        # for e in self.iddefs:
        #     if e.default.isSome:
        #         e.pat.asign(e.default.get.eval(env, global))
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
