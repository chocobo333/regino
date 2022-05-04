
import options
import sequtils
import strformat
import macros
import tables
import sets

import ../../il except Var
import ../../utils
import ../../orders


type
    RegionEnv* = ref object
        constraints: seq[Constraint]
        rvs: seq[Region]
        order: Order[Region]
    Constraint = (Region, Region)


{.experimental: "caseStmtMacros".}

import std/macros

macro `case`(n: tuple): untyped =
    ## this code is from https://nim-lang.org/docs/manual_experimental.html#case-statement-macros
    result = newTree(nnkIfStmt)
    let selector = n[0]
    for i in 1 ..< n.len:
        let it = n[i]
        case it.kind
        of nnkElse, nnkElifBranch, nnkElifExpr, nnkElseExpr:
            result.add it
        of nnkOfBranch:
            for j in 0..it.len-2:
                let cond = newCall("==", selector, it[j])
                result.add newTree(nnkElifBranch, cond, it[^1])
        else:
            error "custom 'case' for tuple cannot handle this node", it

proc `<=`*(r1, r2: Region): bool {.error.}
proc `<=`*(self: RegionEnv, r1, r2: Region): bool =
    # return r2 <= r1
    template `<=`(r1, r2: Region): untyped =
        self.`<=`(r1, r2)
    case (r1.kind, r2.kind)
    of (RegionKind.Param, RegionKind.Param):
        assert false, "notimplemented"
        false
    of (RegionKind.Param, RegionKind.Return):
        assert false, "notimplemented"
        false
    of (RegionKind.Param, RegionKind.Suite):
        assert false, "notimplemented"
        false
    of (RegionKind.Param, RegionKind.Var):
        assert false, "notimplemented"
        false
    of (RegionKind.Return, RegionKind.Param):
        self.order.contains((r1, r2))
    of (RegionKind.Return, RegionKind.Return):
        assert false, "notimplemented"
        false
    of (RegionKind.Return, RegionKind.Suite):
        # TODO: is this truth?
        not(r2 <= r1)
    of (RegionKind.Return, RegionKind.Var):
        r1 <= r2.lb
    of (RegionKind.Suite, RegionKind.Param):
        assert false, "notimplemented"
        false
    of (RegionKind.Suite, RegionKind.Return):
        r1 <= r2.lb
    of (RegionKind.Suite, RegionKind.Suite):
        if r1.parent == r2:
            true
        else:
            r1.parent <= r2
    of (RegionKind.Suite, RegionKind.Var):
        r1 <= r2.lb
    of (RegionKind.Var, RegionKind.Param):
        assert false, "notimplemented"
        false
    of (RegionKind.Var, RegionKind.Return):
        assert false, "notimplemented"
        false
    of (RegionKind.Var, RegionKind.Suite):
        assert false, "notimplemented"
        false
    of (RegionKind.Var, RegionKind.Var):
        false
    else:
        if r1.kind == RegionKind.Link:
            r1.to <= r2
        elif r2.kind == RegionKind.Link:
            r1 <= r2.to
        elif r1.kind == RegionKind.Static:
            true
        elif r2.kind == RegionKind.Static:
            true
        elif r2.kind == RegionKind.Global:
            true
        elif r1.kind == RegionKind.Global:
            false
        else:
            assert false, "cannot reach"
            false

proc Var*(_: typedesc[Region], lb: Region, env: RegionEnv): Region =
    result = il.Var(Region, lb)
    env.rvs.add result

proc isStatic(self: Region): bool =
    self.kind == RegionKind.Static
proc isGlobal(self: Region): bool =
    self.kind == RegionKind.Global
proc coerce(self: RegionEnv, r1, r2: Region) =
    if r1.isStatic:
        return
    if r2.isStatic:
        return
    if r2.isGlobal:
        return
    self.order.add (r1, r2)
proc coerce(self: RegionEnv, r1: Region, t2: Value) =
    # TODO: consider aboud tuple, record and so on
    self.coerce(r1, t2.region)
proc coerce(self: RegionEnv, t1, t2: Value) =
    if t1.kind == t2.kind:
        self.coerce(t1.region, t2.region)
        case t1.kind
        of ValueKind.Literal:
            assert false, "notimplementd"
        of ValueKind.Bottom:
            assert false, "notimplementd"
        of ValueKind.Unit:
            assert false, "notimplementd"
        of ValueKind.Bool:
            assert false, "notimplementd"
        of ValueKind.Integer:
            discard
        of ValueKind.Float:
            assert false, "notimplementd"
        of ValueKind.Char:
            assert false, "notimplementd"
        of ValueKind.String:
            discard
        of ValueKind.Pair:
            assert false, "notimplementd"
        of ValueKind.Array:
            assert false, "notimplementd"
        of ValueKind.ArrayV:
            assert false, "notimplementd"
        of ValueKind.Record:
            assert false, "notimplementd"
        of ValueKind.Ptr:
            assert false, "notimplementd"
        of ValueKind.Pi:
            assert false, "notimplementd"
        of ValueKind.Family:
            assert false, "notimplementd"
        of ValueKind.Sum:
            assert false, "notimplementd"
        of ValueKind.Trait:
            assert false, "notimplementd"
        of ValueKind.Singleton:
            assert false, "notimplementd"
        of ValueKind.Distinct:
            assert false, "notimplementd"
        of ValueKind.Intersection:
            assert false, "notimplementd"
        of ValueKind.Union:
            assert false, "notimplementd"
        of ValueKind.Cons:
            assert false, "notimplementd"
        of ValueKind.Lambda:
            assert false, "notimplementd"
        of ValueKind.Select:
            assert false, "notimplementd"
        of ValueKind.Var:
            assert false, "notimplementd"
        of ValueKind.Gen:
            assert false, "notimplementd"
        of ValueKind.Link:
            assert false, "notimplementd"
    else:
        assert false, "notimplemented"

macro coerce(self: RegionEnv, rel: untyped): untyped =
    rel.expectKind(nnkInfix)
    let
        (op, l, r) = (rel[0], rel[1], rel[2])
    if op.strVal == "<=":
        quote:
            `self`.coerce(`l`, `r`)
    else:
        error fmt"permitted is {l} <= {r}.", op
        newEmptyNode()

proc resolve*(self: RegionEnv, r1, r2: Region) =
    template `<=`(r1, r2: Region): untyped =
        self.`<=`(r1, r2)
    # if r1 <= r2:
    #     return
    case (r1.kind, r2.kind)
    of (RegionKind.Static, RegionKind.Static):
        assert false, "notimplemented"
    of (RegionKind.Static, RegionKind.Global):
        assert false, "notimplemented"
    of (RegionKind.Static, RegionKind.Param):
        assert false, "notimplemented"
    of (RegionKind.Static, RegionKind.Return):
        assert false, "notimplemented"
    of (RegionKind.Static, RegionKind.Suite):
        assert false, "notimplemented"
    of (RegionKind.Static, RegionKind.Var):
        assert false, "notimplemented"
    of (RegionKind.Global, RegionKind.Static):
        assert false, "notimplemented"
    of (RegionKind.Global, RegionKind.Global):
        assert false, "notimplemented"
    of (RegionKind.Global, RegionKind.Param):
        assert false, "notimplemented"
    of (RegionKind.Global, RegionKind.Return):
        assert false, "notimplemented"
    of (RegionKind.Global, RegionKind.Suite):
        assert false, "notimplemented"
    of (RegionKind.Global, RegionKind.Var):
        if not(r1 <= r2) and r2.lb <= r1:
            r2.lb = r1
        # r2.lb = glb(r1, r2.lb)
    of (RegionKind.Param, RegionKind.Static):
        assert false, "notimplemented"
    of (RegionKind.Param, RegionKind.Global):
        assert false, "notimplemented"
    of (RegionKind.Param, RegionKind.Param):
        assert false, "notimplemented"
    of (RegionKind.Param, RegionKind.Return):
        assert false, "notimplemented"
    of (RegionKind.Param, RegionKind.Suite):
        assert false, "notimplemented"
    of (RegionKind.Param, RegionKind.Var):
        assert false, "notimplemented"
    of (RegionKind.Return, RegionKind.Static):
        assert false, "notimplemented"
    of (RegionKind.Return, RegionKind.Global):
        assert false, "notimplemented"
    of (RegionKind.Return, RegionKind.Param):
        self.order.add (r1, r2)
        self.constraints.add (r1, r2)
    of (RegionKind.Return, RegionKind.Return):
        assert false, "notimplemented"
    of (RegionKind.Return, RegionKind.Suite):
        assert false, "notimplemented"
    of (RegionKind.Return, RegionKind.Var):
        self.constraints.add (r1, r2)
        if not(r1 <= r2) and r2.lb <= r1:
            r2.lb = r1
        # r2.lb = glb(r1, r2.lb)
    of (RegionKind.Suite, RegionKind.Static):
        assert false, "notimplemented"
    of (RegionKind.Suite, RegionKind.Global):
        assert false, "notimplemented"
    of (RegionKind.Suite, RegionKind.Param):
        assert false, "notimplemented"
    of (RegionKind.Suite, RegionKind.Return):
        assert false, "notimplemented"
    of (RegionKind.Suite, RegionKind.Suite):
        assert false, "notimplemented"
    of (RegionKind.Suite, RegionKind.Var):
        if not(r1 <= r2) and r2.lb <= r1:
            r2.lb = r1
        # r2.lb = glb(r1, r2.lb)
    of (RegionKind.Var, RegionKind.Static):
        assert false, "notimplemented"
    of (RegionKind.Var, RegionKind.Global):
        assert false, "notimplemented"
    of (RegionKind.Var, RegionKind.Param):
        assert false, "notimplemented"
    of (RegionKind.Var, RegionKind.Return):
        assert false, "notimplemented"
    of (RegionKind.Var, RegionKind.Suite):
        assert false, "notimplemented"
    of (RegionKind.Var, RegionKind.Var):
        if not (r1.lb <= r2.lb) and r2.lb <= r1.lb:
            r2.lb = r1.lb
    else:
        if r1.kind == RegionKind.Link:
            self.resolve(r1.to, r2)
        if r2.kind == RegionKind.Link:
            self.resolve(r1, r2.to)

proc bindrv(self: RegionEnv, rv: Region) =
    assert rv.kind == RegionKind.Var
    let
        (_, dests) = self.order.clear(rv)
    rv[] = rv.lb[]
    for e in dests:
        self.order.add (rv, e)
proc collapse(self: RegionEnv, cycle: seq[Region]) =
    debug cycle
    assert false, "notimplemented"
proc resolve*(self: RegionEnv, r: Region) =
    if r in self.order.dual:
        for e in self.order.dual[r]:
            self.order.remove((e, r))
    case r.kind
    of RegionKind.Static:
        discard
    of RegionKind.Global:
        discard
    of RegionKind.Param:
        discard
    of RegionKind.Return:
        discard
    of RegionKind.Suite:
        discard
    of RegionKind.Var:
        self.bindrv(r)
    of RegionKind.Link:
        discard
    if r in self.order.primal:
        for e in self.order.primal[r]:
            self.resolve(r, e)
proc resolve*(self: RegionEnv) =
    for k in self.order.primal.keys:
        for v in self.order.primal[k]:
            let cycle = self.order.path(v, k)
            if cycle.isSome:
                let cycle = cycle.get.mapIt(it[0])
                self.collapse(cycle)
    for e in self.order.sort:
        self.resolve(e)


proc hasRegion*(self: Value): bool =
    case self.kind
    of ValueKind.Link:
        self.to.hasRegion
    of ValueKind.String:
        true
    of ValueKind.Pair:
        false
    of ValueKind.Singleton, ValueKind.Distinct:
        self.base.hasRegion
    of ValueKind.Pi, ValueKind.Ptr:
        true
    else:
        false
proc hasRegionRecursive(self: Value): bool =
    case self.kind
    of ValueKind.Link:
        self.to.hasRegionRecursive
    of ValueKind.String:
        true
    of ValueKind.Pair:
        self.first.hasRegionRecursive or self.second.hasRegionRecursive
    of ValueKind.Singleton, ValueKind.Distinct:
        self.base.hasRegionRecursive
    of ValueKind.Pi, ValueKind.Ptr:
        true
    else:
        false

proc LetSymbol(self: Pattern, env: RegionEnv, suite: Region) =
    # TODO: Pattern has no region?
    let r = case self.kind
    of PatternKind.Literal:
        assert false, "notimplemented"
        Region.Static
    of PatternKind.Ident:
        assert self.typ.symbol.isSome, "internal error"
        let r = if self.typ.hasRegion:
            Region.Var(suite, env)
        else:
            Region.Static
        self.typ.symbol.get.typ.region = r
        self.ident.typ.region = r
        r
    of PatternKind.Tuple:
        for e in self.patterns:
            e.LetSymbol(env, suite)
        Region.Static
    of PatternKind.Record:
        assert false, "notimplemented"
        Region.Static
    of PatternKind.UnderScore:
        assert false, "notimplemented"
        Region.Static
    self.typ.region = r
proc ParamSymbol(self: seq[Pattern], env: RegionEnv, suite: Region) =
    let ids = self.mapIt(it.collectIdent).flatten
    for (i, id) in ids.pairs:
        assert id.typ.symbol.isSome, "internal error"
        let r = Region.Param(suite, i)
        id.typ.symbol.get.typ.region = r
        id.typ.region = r

proc infer(self: Value, env: RegionEnv, suite: Region) =
    if not self.region.isNil:
        return
    # TODO: is it true that only ident has a symbol?
    if self.symbol.isSome:
        self.region = self.symbol.get.typ.region
        return
    self.region = case self.kind
    of ValueKind.Literal:
        Region.Static
    of ValueKind.Bottom:
        Region.Static
    of ValueKind.Unit:
        Region.Static
    of ValueKind.Bool:
        Region.Static
    of ValueKind.Integer:
        Region.Static
    of ValueKind.Float:
        Region.Static
    of ValueKind.Char:
        Region.Static
    of ValueKind.String:
        Region.Var(suite, env)
    of ValueKind.Pair:
        self.first.infer(env, suite)
        self.second.infer(env, suite)
        Region.Static
    of ValueKind.Array, ValueKind.Singleton, ValueKind.Distinct:
        self.base.infer(env, suite)
        Region.Static
    of ValueKind.ArrayV:
        Region.Static
    of ValueKind.Record:
        Region.Static
    of ValueKind.Ptr:
        Region.Var(suite, env)
    of ValueKind.Pi:
        Region.Static
    of ValueKind.Family:
        Region.Static
    of ValueKind.Sum:
        Region.Static
    of ValueKind.Trait:
        Region.Static
    of ValueKind.Intersection:
        assert false, "cant reach here"
        Region.Static
    of ValueKind.Union:
        assert false, "cant reach here"
        Region.Static
    of ValueKind.Cons:
        Region.Static
    of ValueKind.Lambda:
        Region.Static
    of ValueKind.Var:
        assert false, "cant reach here"
        Region.Static
    of ValueKind.Select:
        assert false, "cant reach here"
        Region.Static
    of ValueKind.Gen:
        Region.Static
    of ValueKind.Link:
        self.to.infer(env, suite)
        self.to.region
proc infer(self: Suite, env: RegionEnv, suite: Region)
proc infer(self: Function, env: RegionEnv, suite: Region)
proc infer(self: Expression, env: RegionEnv, suite: Region) =
    self.typ.infer(env, suite)
    case self.kind
    of ExpressionKind.Literal:
        discard
    of ExpressionKind.Ident:
        discard
    of ExpressionKind.Tuple:
        for e in self.exprs:
            e.infer(env, suite)
    of ExpressionKind.Array:
        # TODO: add region constraints
        for e in self.exprs:
            e.infer(env, suite)
    of ExpressionKind.Record:
        discard
    of ExpressionKind.If:
        for e in self.elifs:
            let
                cond = e.cond
                bl = e.suite
            cond.infer(env, suite)
            bl.infer(env, suite)
        if self.elseb.isSome:
            self.elseb.get.infer(env, suite)
    of ExpressionKind.When:
        discard
    of ExpressionKind.Case:
        discard
    of ExpressionKind.Call, ExpressionKind.Command:
        self.callee.infer(env, suite)
        for e in self.args:
            e.infer(env, suite)
        debug self.callee.typ.symbol.get.constraints
    of ExpressionKind.Dot:
        discard
    of ExpressionKind.Bracket:
        discard
    of ExpressionKind.Binary:
        discard
    of ExpressionKind.Prefix:
        discard
    of ExpressionKind.Postfix:
        discard
    of ExpressionKind.Block:
        self.`block`.infer(env, suite)
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
        discard
proc infer(self: Pattern, env: RegionEnv, suite: Region) =
    self.typ.infer(env, suite)
    case self.kind
    of PatternKind.Literal:
        discard
    of PatternKind.Ident:
        discard
    of PatternKind.Tuple:
        for e in self.patterns:
            e.infer(env, suite)
    of PatternKind.Record:
        discard
    of PatternKind.UnderScore:
        discard
proc infer(self: Statement, env: RegionEnv, suite: Region) =
    self.typ.infer(env, suite)
    case self.kind
    of StatementKind.For:
        discard
    of StatementKind.While:
        discard
    of StatementKind.Loop:
        discard
    of StatementKind.LetSection, StatementKind.VarSection:
        for e in self.iddefs:
            e.pat.LetSymbol(env, suite)
            if e.default.isSome:
                e.default.get.infer(env, suite)
                # TODO: Pattern has no region?
                env.coerce(e.pat.typ <= e.default.get.typ)
    of StatementKind.ConstSection:
        discard
    of StatementKind.TypeSection:
        discard
    of StatementKind.Asign:
        self.pat.infer(env, suite)
        self.val.infer(env, suite)
        env.coerce(self.pat.typ <= self.val.typ)
    of StatementKind.Funcdef:
        self.fn.infer(env, suite)
    of StatementKind.Meta:
        discard
    of StatementKind.Discard:
        discard
    of StatementKind.Comments:
        discard
    of StatementKind.Expression:
        self.expression.infer(env, suite)
    of StatementKind.Fail:
        discard

proc Suite(_: typedesc[Value], parent: Region): Value =
    result = Value.Unit
    result.region = Region.Suite(parent)
proc infer(self: Suite, env: RegionEnv, suite: Region) =
    let suite = Region.Suite(suite)
    for s in self.stmts:
        s.infer(env, suite)
var f = 0
proc infer(self: Function, env: RegionEnv, suite: Region) =
    let env = RegionEnv()
    self.param.params.mapIt(it.pat).ParamSymbol(env, suite)
    if self.suite.isSome:
        self.suite.get.infer(env, suite)
        # TODO: consider when self.suite.get.typ was complicated
        env.coerce(Region.Return(suite), self.suite.get.typ)
    env.order.dot(fmt"src/v2/sema/region/graphs/f{f}.md")
    inc f
    env.resolve
    self.id.typ.symbol.get.constraints = env.constraints

proc infer(self: Program, env: RegionEnv) =
    for s in self.stmts:
        s.infer(env, Region.Global)
    env.resolve

when isMainModule:
    import ../../parsers
    import ../../sema

    let
        src = """
type
    int = typeof(0)
    bool = typeof(false)
    string = typeof("")
func `eq@bool/int32int32`(a: int, b: int) -> bool ![importll]
func `==`(a: int, b: int) -> bool:
    `eq@bool/int32int32`(a, b)
func `add@int32/int32int32`(a: int, b: int) -> int ![importll]
func `+`(a: int, b: int) -> int:
    `add@int32/int32int32`(a, b)
func echo(a: string) ![importll]
func fib(n: int) -> int:
    if n == 0:
        1
    elif n == 1:
        1
    else:
        fib(n-1) + fib(n-2)
func f(s: string) -> string:
    s
func f1():
    var a: string
    block:
        a = "string"
    echo a
func f2(s: string) -> string:
    var a: string = s
    block:
        a = "string"
    a
func test((a, b): (int, int), c: bool):
    discard
let
    a = 3
    b = "abc"
a = 5
echo f("ff")
(a, b)
"""
        program = Program(Source.from(src)).get
        env = RegionEnv()
    echo program.sema
    echo program
    program.infer(env)
    echo program
    echo env.order
    env.order.dot("src/v2/sema/region/graph.md")

    let
        r1 = Region.Suite(Region.Global)
        r2 = Region.Suite(Region.Suite(Region.Global))
    echo env.`<=`(r1, r2)
