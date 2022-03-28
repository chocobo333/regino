
import options
import sequtils
import sugar
import sets

import ../il
import ../typeenv

import infer as _


proc eval*(self: Program)
proc eval*(self: Statement, env: TypeEnv, global: bool = false): Value
proc eval*(self: Expression, env: TypeEnv, global: bool = false): Value
proc eval*(self: TypeExpression, env: TypeEnv, global: bool = false): Value
proc infer*(self: Literal): Value =
    self.typ
proc infer*(self: Statement, env: TypeEnv, global: bool = false): Value
proc infer*(self: Suite, env: TypeEnv): Value =
    if self.stmts.len == 0:
        return Value.Unit
    env.enter(self.scope):
        for s in self.stmts[0..^2]:
            env.coerce(s.infer(env) == Value.Unit)
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
    case syms.len
    of 0:
        Value.Var(env)
    of 1:
        syms[0].typ.inst(env)
    else:
        Value.Intersection(syms.mapIt(it.typ.inst(env)))
proc infer*(self: Expression, env: TypeEnv, global: bool = false): Value =
    result = case self.kind
    of ExpressionKind.Literal:
        self.litval.infer
    of ExpressionKind.Ident:
        self.ident.infer(env, global)
    of ExpressionKind.Tuple:
        self.exprs.mapIt(it.infer(env, global)).foldl(Value.Pair(a, b))
    of ExpressionKind.Seq:
        Value.Unit
    of ExpressionKind.Record:
        Value.Unit
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
        tv
    of ExpressionKind.Dot:
        Value.Unit
    of ExpressionKind.Bracket:
        Value.Unit
    of ExpressionKind.Binary:
        let
            tv = Value.Var(env)
            lhs = self.lhs.infer(env, global)
            rhs = self.rhs.infer(env, global)
            op = self.op.infer(env, global)
        env.coerce(op <= Value.Arrow(@[lhs, rhs], tv))
        tv
    of ExpressionKind.Prefix:
        Value.Unit
    of ExpressionKind.Postfix:
        Value.Unit
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
            let
                syms = env.lookupId(self.ident.name)
            case syms.len
            of 0:
                Value.Var(env)
            of 1:
                syms[0].typ.inst(env)
            else:
                Value.Intersection(syms.mapIt(it.typ.inst(env)))
        else:
            let res = Value.Var(env)
            # self.ident.typ = res
            res
    of PatternKind.Dot:
        Value.Unit # TODO:
    of PatternKind.Tuple:
        self.patterns.mapIt(it.infer(env, global)).foldl(Value.Pair(a, b))
    of PatternKind.Record:
        Value.Unit
    of PatternKind.UnderScore:
        Value.Var(env)
    self.typ = result
proc addPat(env: TypeEnv, impl: IdentDef, pat: Pattern = impl.pat, global: bool = false) =
    ## register identifier on typeenv
    case pat.kind
    of PatternKind.Literal:
        discard
    of PatternKind.Ident:
        # TODO: index
        let sym = Symbol.Let(pat.ident, pat.typ, impl, global)
        env.addIdent(sym)
    of PatternKind.Dot:
        discard # TODO:
    of PatternKind.Tuple:
        for pat in pat.patterns:
            env.addPat(impl, pat, global)
    of PatternKind.Record:
        discard
    of PatternKind.UnderScore:
        discard
proc PairToSeq(pair: Value): seq[Value] =
    if pair.kind == ValueKind.Pair:
        @[pair.first] & pair.second.PairToSeq
    else:
        @[pair]
proc addFunc(env: TypeEnv, fn: Function, global: bool = false) =
    proc addPat(env: TypeEnv, impl: IdentDef, typ: Value, pat: Pattern = impl.pat, global: bool = false) =
        ## register identifier on typeenv
        case pat.kind
        of PatternKind.Literal:
            discard
        of PatternKind.Ident:
            # TODO: index
            let sym = Symbol.Param(pat.ident, typ, impl, global)
            env.addIdent(sym)
        of PatternKind.Dot:
            discard # TODO:
        of PatternKind.Tuple:
            assert typ.kind == ValueKind.Pair
            for (pat, typ) in pat.patterns.zip(typ.PairToSeq):
                env.addPat(impl, typ, pat, global)
        of PatternKind.Record:
            discard
        of PatternKind.UnderScore:
            discard
        pat.typ = typ
    var
        sym: Symbol
        rety: Value
    env.enter(fn.param.scope):
        let
            implicit = fn.param.implicit.mapIt(block:
                let
                    id = it.id
                    ub = it.ub.map(it => it.eval(env, global)).get(Value.Unit)
                    genty = GenericType(id: id, ub: ub, typ: ub.typ)
                    typ = Value.Gen(genty)
                    sym = Symbol.GenParam(id, typ, it)
                env.addIdent(sym)
                genty
            )
            paramty = fn.param.params.mapIt(block:
                # TODO: pattern like a, b: int
                assert it.typ.isSome
                let paramty = it.typ.get.eval(env, global)
                env.addPat(it, paramty)
                paramty
            )
        rety = fn.param.rety.map(it => it.eval(env, global)).get(Value.Unit)
        sym = Symbol.Func(fn.id, Value.Pi(implicit, paramty, rety), global)
    env.addIdent(sym)
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
                let t = typ.get.eval(env, global)
                env.coerce(t == paty)
            env.addPat(iddef, global=global)
        Value.Unit
    of StatementKind.VarSection:
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
                let
                    typ = typ.eval(env, global)
                    sym = Symbol.Typ(id, typ, typedef, global)
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
    if self.stmts.len == 0:
        return Value.Unit
    for s in self.stmts[0..^2]:
        env.coerce(s.infer(env) == Value.Unit)
    result = self.stmts[^1].infer(env)
    echo env.constraints
    env.resolveRelations()
    echo env.constraints
    echo env.tvs

proc eval*(self: Literal): Value =
    Value.literal(self)
proc eval*(self: TypeExpression, env: TypeEnv, global: bool = false): Value =
    case self.kind
    of TypeExpressionKind.Object:
        Value.Unit
    of TypeExpressionKind.Sum:
        Value.Unit
    of TypeExpressionKind.Distinct:
        Value.Unit
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
    of ExpressionKind.Seq:
        Value.Unit
    of ExpressionKind.Record:
        Value.Unit
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
    of ExpressionKind.Call:
        Value.Unit
    of ExpressionKind.Command:
        Value.Unit
    of ExpressionKind.Dot:
        Value.Unit
    of ExpressionKind.Bracket:
        Value.Unit
    of ExpressionKind.Binary:
        Value.Unit
    of ExpressionKind.Prefix:
        Value.Unit
    of ExpressionKind.Postfix:
        Value.Unit
    of ExpressionKind.Block:
        self.`block`.infer(env)
    of ExpressionKind.Lambda:
        Value.Unit
    of ExpressionKind.Malloc:
        env.coerce(self.msize.infer(env, global) == Value.Integer)
        Value.Ptr(self.mtype.eval(env, global))
    of ExpressionKind.Typeof:
        self.`typeof`.infer(env, global)
    of ExpressionKind.Ref:
        Value.Unit
    of ExpressionKind.FnType:
        Value.Unit
    of ExpressionKind.Fail:
        Value.Bottom
proc eval*(self: Statement, env: TypeEnv, global: bool = false): Value =
    Value.Unit
proc eval*(self: Program) =
    discard
