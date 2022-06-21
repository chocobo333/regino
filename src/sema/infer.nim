
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


proc infer*(self: Statement, env: TypeEnv, global: bool = false): Value
proc infer*(self: Suite, env: TypeEnv): Value
proc infer*(self: ElifBranch, env: TypeEnv, global: bool = false): Value

proc infer*(self: Literal): Value =
    ## that retrurns literal's type merely
    self.typ
proc infer*(self: Ident, env: TypeEnv, global: bool = false): Value =
    if not self.typ.isNil:
        return self.typ
    let
        syms = env.lookupId(self.name)
    result = case syms.len
    of 0:
        Value.Var(env)
    of 1:
        syms[0].typ.inst(env)
    else:
        Value.Select(syms.mapIt(it.typ.inst(env)), env)
        # Value.Intersection(syms.mapIt(it.typ.inst(env)))
    self.typ = result
proc infer*(self: Expression, env: TypeEnv, global: bool = false): Value {.exportc: "infer_e".} =
    if not self.typ.isNil:
        return self.typ
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
    of ExpressionKind.Case:
        Value.Unit
    of ExpressionKind.Call, ExpressionKind.Command:
        let
            tv = Value.Var(env)
            args = self.args.mapIt(it.infer(env, global))
            callee = self.callee.infer(env, global)
        env.coerce(callee <= Value.Arrow(args, tv))
        env.coerce(Value.Arrow(args.mapIt(Value.Unit), tv) <= callee) # i dont know whether this is correct.
        tv
    of ExpressionKind.Dot:
        let
            tv = Value.Var(env)
            args = @[self.lhs.infer(env, global)]
            callee = self.rhs.infer(env, global)
        env.coerce(callee <= Value.Arrow(args, tv))
        env.coerce(Value.Arrow(@[Value.Unit], tv) <= callee) # i dont know whether this is correct.
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
        env.coerce(Value.Arrow(@[Value.Unit, Value.Unit], tv) <= op) # i dont know whether this is correct.
        tv
    of ExpressionKind.Prefix, ExpressionKind.Postfix:
        let
            tv = Value.Var(env)
            exp = self.expression.infer(env, global)
            op = self.op.infer(env, global)
        env.coerce(op <= Value.Arrow(@[exp], tv))
        env.coerce(Value.Arrow(@[Value.Unit], tv) <= op)
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
    of ExpressionKind.IntCast:
        Value.Integer(self.to)
    of ExpressionKind.Fail:
        Value.Bottom
    self.typ = result
proc infer*(self: Metadata, env: TypeEnv, global: bool = false): Value =
    for param in self.params:
        discard param.infer(env)
    Value.Unit
proc infer*(self: Pattern, env: TypeEnv, global: bool = false, asign: bool = false): Value =
    if not self.typ.isNil:
        return self.typ
    result = case self.kind
    of PatternKind.Literal:
        self.litval.infer
    of PatternKind.Ident:
        # TODO: index
        if asign:
            let res = self.ident.infer(env, global)
            if res.kind == ValueKind.Select:
                toSeq(res.types.filter(it => it.symbol.get.kind != SymbolKind.Func))[0]
            else:
                res
        else:
            let res = Value.Var(env)
            self.ident.typ = res
            res
    of PatternKind.Tuple:
        self.patterns.mapIt(it.infer(env, global, asign)).foldl(Value.Pair(a, b))
    of PatternKind.Record:
        Value.Record(self.members.mapIt((it[0], it[1].infer(env, global, asign))).toTable)
    of PatternKind.UnderScore:
        Value.Var(env)
    self.typ = result
proc infer(self: TypeExpression, env: TypeEnv, ident: Ident): Value =
    case self.kind
    of TypeExpressionKind.Object:
        # TODO: ituka jissousuru
        let rec = Value.Record(self.members.mapIt((it[0], it[1].infer(env, it[0]))).toTable)
        Value.Distinct(ident, rec)
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
        Value.Distinct(ident, self.base.infer(env, ident))
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
        let sym = Symbol.Let(pat.ident, pat.typ, impl, global)
        env.addIdent(sym)
    of PatternKind.Tuple:
        for pat in pat.patterns:
            env.addPatL(impl, pat, global)
    of PatternKind.Record:
        for (id, pat) in pat.members:
            env.addPatL(impl, pat, global)
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
            env.addIdent(sym)
        of PatternKind.Tuple:
            assert typ.kind == ValueKind.Pair
            for (p, typ) in pat.patterns.zip(typ.PairToSeq):
                env.addParam(impl, typ, p, global)
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
        sym = Symbol.Func(fn.id, fnty, fn, global)
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

proc addTypeExpr(self: TypeEnv, typ: Value, typeExpr: TypeExpression, global: bool = false) =
    case typeExpr.kind
    of TypeExpressionKind.Object:
        var i = 0
        for (id, t) in typeExpr.members.sortedByIt(it[0].name):
            let
                field = Value.Arrow(@[typ], typ.base.members[id])
                sym = Symbol.Field(id, field, i, (id, t), global)
            self.addIdent(sym)
            inc i
    of TypeExpressionKind.Sum:
        for cons in typeExpr.sum.constructors:
            case cons.kind
            of SumConstructorKind.NoField:
                let
                    id = cons.id
                    sym = Symbol.Enum(id, Value.Link(typ), cons, global)
                self.addIdent(sym)
            of SumConstructorKind.UnnamedField:
                let
                    id = cons.id
                    typ =
                        case typ.base.constructors[id].tupleLen
                        of 0:
                            Value.Arrow(@[], typ)
                        of 1:
                            Value.Arrow(@[typ.base.constructors[id].first], typ)
                        else:
                            Value.Arrow(typ.base.constructors[id].PairToSeq, typ)
                    sym = Symbol.Enum(id, typ, cons, global)
                self.addIdent(sym)
            of SumConstructorKind.NamedField:
                let
                    id = cons.id
                debug typ.base.constructors[id]
                # TODO: object construction
    of TypeExpressionKind.Distinct:
        discard
    of TypeExpressionKind.Trait:
        discard
    of TypeExpressionKind.Expression:
        discard

proc infer*(self: IdentDefSection, env: TypeEnv, global: bool = false) =
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
proc infer*(self: TypeDefSection, env: TypeEnv, global: bool = false) =
    for typedef in self.typedefs:
        let
            id = typedef.id
            params = typedef.params
            typ = typedef.typ
            tv = Value.Var(env)
            sym = Symbol.Typ(id, tv, typedef, global)
        env.addIdent(sym)
        if params.isNone:
            # TODO: infer and check
            let _ = typ.infer(env, id)
            # typ.check(env)
            let
                typ = typ.eval(env, id, global)
            env.bindtv(tv, typ)
        else:
            let scope = newScope(env.scope)
            env.enter scope:
                let implicit = params.get.mapIt(it.infer(env, global))
                let
                    _ = typ.infer(env, id)
                    typ = typ.eval(env, id, global)
                env.bindtv(tv, Value.Family(implicit, typ))
        env.addTypeExpr(tv, typ, global)
proc infer*(self: Statement, env: TypeEnv, global: bool = false): Value {.exportc: "infer_s".} =
    if not self.typ.isNil:
        return self.typ
    result = case self.kind
    of StatementKind.Import:
        Value.Unit
        # TODO:
    of StatementKind.For:
        Value.Unit
    of StatementKind.While:
        let branch = self.branch.infer(env)
        env.coerce(branch == Value.Unit)
        Value.Unit
    of StatementKind.Loop:
        Value.Unit
    of StatementKind.LetSection:
        self.iddefSection.infer(env, global)
        Value.Unit
    of StatementKind.VarSection:
        self.iddefSection.infer(env, global)
        Value.Unit
    of StatementKind.ConstSection:
        Value.Unit
    of StatementKind.TypeSection:
        self.typedefSection.infer(env, global)
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
    self.typ = result
proc infer*(self: Suite, env: TypeEnv): Value =
    if self.stmts.len == 0:
        return Value.Unit
    env.enter(self.scope):
        for s in self.stmts[0..^2]:
            discard s.infer(env)
            # env.coerce(s.infer(env) == Value.Unit, TypeError.Discard(s))
        result = self.stmts[^1].infer(env)
    # env.resolveRelationsPartially()
proc infer*(self: ElifBranch, env: TypeEnv, global: bool = false): Value =
    let
        cond = self.cond.infer(env, global)
    env.coerce(cond == Value.Bool)
    self.suite.infer(env)
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
    env.resolve()
