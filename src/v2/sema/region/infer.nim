
import options
import sequtils
import strformat
import macros

import ../../il


type
    RegionEnv* = ref object
        constraints: seq[Constraint]
        rvs: seq[Region]
    Constraint = (Value, Value)

proc Var*(_: typedesc[Region], lb: Region, env: RegionEnv): Region =
    result = Region(kind: RegionKind.Var, lb: lb)
    env.rvs.add result

macro coerce(self: RegionEnv, rel: untyped): untyped =
    rel.expectKind(nnkInfix)
    let
        (op, l, r) = (rel[0], rel[1], rel[2])
    if op.strVal == "<=":
        quote:
            `self`.constraints.add (`l`, `r`)
    else:
        error fmt"permitted is {l} <= {r}.", op
        newEmptyNode()

proc `<=`*(r1, r2: Region): bool =
    # return r2 <= r1
    if r1.kind == RegionKind.Static or r2.kind == RegionKind.Static:
        true
    elif r1.kind == RegionKind.Link:
        r1.to <= r2
    elif r2.kind == RegionKind.Link:
        r1 <= r2.to
    elif r2.kind == RegionKind.Global:
        true
    elif r1.kind == r2.kind:
        case r1.kind
        of RegionKind.Suite:
            if r1.parent == r2:
                true
            else:
                r1.parent <= r2
        else:
            false
    elif r2.kind == RegionKind.Var:
        r1 <= r2.lb
    else:
        false

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

proc resolve*(self: RegionEnv, r1, r2: Region) =
    if r1 <= r2:
        return
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
        assert false, "notimplemented"
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
        assert false, "notimplemented"
    of (RegionKind.Return, RegionKind.Return):
        assert false, "notimplemented"
    of (RegionKind.Return, RegionKind.Suite):
        assert false, "notimplemented"
    of (RegionKind.Return, RegionKind.Var):
        assert false, "notimplemented"
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
        assert false, "notimplemented"
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

proc resolve*(self: RegionEnv, t1, t2: Value) =
    self.resolve(t1.region, t2.region)
proc resolve*(self: RegionEnv): seq[Constraint] =
    while self.constraints.len != 0:
        let (t1, t2) = self.constraints.pop
        self.resolve(t1, t2)
    for e in self.rvs:
        echo e.lb

proc hasRegion(self: Value): bool =
    case self.kind
    of ValueKind.Link:
        self.to.hasRegion
    of ValueKind.String:
        true
    of ValueKind.Pair:
        self.first.hasRegion or self.second.hasRegion
    of ValueKind.Singleton, ValueKind.Distinct:
        self.base.hasRegion
    of ValueKind.Pi, ValueKind.Ptr:
        true
    else:
        false

proc LetSymbol(self: Pattern, env: RegionEnv, suite: Region) =
    case self.kind
    of PatternKind.Literal:
        discard
    of PatternKind.Ident:
        assert self.typ.symbol.isSome, "internal error"
        self.typ.symbol.get.region = Region.Var(suite)
    of PatternKind.Tuple:
        for e in self.patterns:
            e.LetSymbol(env, suite)
    of PatternKind.Record:
        discard
    of PatternKind.UnderScore:
        discard

proc infer(self: Value, env: RegionEnv, suite: Region) =
    if not self.region.isNil:
        return
    # TODO: is it true that only ident has a symbol?
    if self.symbol.isSome:
        self.region = self.symbol.get.region
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
    of ValueKind.Var:
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
        discard
    of ExpressionKind.When:
        discard
    of ExpressionKind.Case:
        discard
    of ExpressionKind.Call:
        discard
    of ExpressionKind.Command:
        discard
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
proc infer(self: Function, env: RegionEnv, suite: Region) =
    if self.suite.isSome:
        self.suite.get.infer(env, suite)
proc infer(self: Program, env: RegionEnv) =
    for s in self.stmts:
        s.infer(env, Region.Global)
    let cons = env.resolve
    assert cons.len == 0

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
func f2() -> string:
    var a: string
    block:
        a = "string"
    a
let
    a = 3
a = 5
a
"""
        program = Program(Source.from(src)).get
        env = RegionEnv()
    echo program.sema
    echo program
    program.infer(env)
