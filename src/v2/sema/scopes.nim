
import options

import ../il


proc setScope(self: ElifBranch, parent: Scope)
proc setScope(self: Suite, parent: Scope)
proc setScope(self: OfBranch, parent: Scope) =
    self.suite.setScope(parent)
proc setScope(self: FunctionParam, parent: Scope)
proc setScope(self: Expression, parent: Scope) =
    case self.kind
    of ExpressionKind.Literal:
        discard
    of ExpressionKind.Ident:
        discard
    of ExpressionKind.Tuple, ExpressionKind.Array:
        for e in self.exprs:
            e.setScope(parent)
    of ExpressionKind.Record:
        for (_, e) in self.members:
            e.setScope(parent)
    of ExpressionKind.If, ExpressionKind.When:
        for eli in self.elifs:
            eli.setScope(parent)
        if self.elseb.isSome:
            self.elseb.get.setScope(parent)
    of ExpressionKind.Case:
        self.val.setScope(parent)
        for e in self.ofs:
            e.setScope(parent)
        if self.default.isSome:
            self.default.get.setScope(parent)
    of ExpressionKind.Call, ExpressionKind.Command, ExpressionKind.Bracket:
        self.callee.setScope(parent)
        for arg in self.args:
            arg.setScope(parent)
    of ExpressionKind.Dot, ExpressionKind.Binary:
        self.lhs.setScope(parent)
        self.rhs.setScope(parent)
    of ExpressionKind.Prefix, ExpressionKind.Postfix:
        self.expression.setScope(parent)
    of ExpressionKind.Block:
        self.`block`.setScope(parent)
    of ExpressionKind.Lambda:
        self.param.setScope(parent)
        self.body.setScope(parent)
    of ExpressionKind.Malloc:
        self.mtype.setScope(parent)
        self.msize.setScope(parent)
    of ExpressionKind.Typeof:
        self.`typeof`.setScope(parent)
    of ExpressionKind.Ref:
        self.`ref`.setScope(parent)
    of ExpressionKind.FnType:
        for arg in self.args:
            arg.setScope(parent)
        self.rety.setScope(parent)
    of ExpressionKind.Fail:
        discard
proc setScope(self: Pattern, parent: Scope) =
    case self.kind
    of PatternKind.Literal:
        discard
    of PatternKind.Ident:
        if self.index.isSome:
            self.index.get.setScope(parent)
    of PatternKind.Dot:
        self.lhs.setScope(parent)
    # of PatternKind.Bracket:
    #     self.callee.setScope(parent)
    #     for arg in self.args:
    #         arg.setScope(parent)
    of PatternKind.Tuple:
        for pat in self.patterns:
            pat.setScope(parent)
    of PatternKind.Record:
        for (_, t) in self.members:
            t.setScope(parent)
    of PatternKind.UnderScore:
        discard
proc setScope(self: Statement, parent: Scope)
proc setScope(self: Suite, parent: Scope) =
    self.scope = newScope(parent)
    for s in self.stmts:
        s.setScope(self.scope)
proc setScope(self: ElifBranch, parent: Scope) =
    self.suite.setScope(parent)
proc setScope(self: IdentDef, parent: Scope) =
    self.pat.setScope(parent)
    if self.typ.isSome:
        self.typ.get.setScope(parent)
    if self.default.isSome:
        self.default.get.setScope(parent)
proc setScope(self: GenTypeDef, parent: Scope) =
    if self.ub.isSome:
        self.ub.get.setScope(parent)
proc setScope(self: FunctionParam, parent: Scope) =
    for iddef in self.implicit:
        iddef.setScope(parent)
    for iddef in self.params:
        iddef.setScope(parent)
    if self.rety.isSome:
        self.rety.get.setScope(parent)
    self.scope = newScope(parent)
proc setScope(self: Metadata, parent: Scope) =
    for e in self.params:
        e.setScope(parent)
proc setScope(self: Function, parent: Scope) =
    self.param.setScope(parent)
    if self.metadata.isSome:
        self.metadata.get.setScope(parent)
    if self.suite.isSome:
        self.suite.get.setScope(self.param.scope)
proc setScope(self: SumConstructor, parent: Scope) =
    case self.kind
    of SumConstructorKind.NoField:
        discard
    of SumConstructorKind.UnnamedField:
        for t in self.types:
            t.setScope(parent)
    of SumConstructorKind.NamedField:
        for (_, t) in self.fields:
            t.setScope(parent)
proc setScope(self: SumType, parent: Scope) =
    for cons in self.constructors:
        cons.setScope(parent)
proc setScope(self: TypeExpression, parent: Scope)
proc setScope(self: Trait, parent: Scope) =
    case self.kind
    of TraitKind.Is:
        self.pat.setScope(parent)
        self.val.setScope(parent)
    of TraitKind.Func:
        self.fn.setScope(parent)
proc setScope(self: TraitType, parent: Scope) =
    self.pat.setScope(parent)
    self.typ.setScope(parent)
    for trait in self.traits:
        trait.setScope(parent)
proc setScope(self: TypeExpression, parent: Scope) =
    case self.kind
    of TypeExpressionKind.Object:
        for (_, e) in self.members:
            e.setScope(parent)
    of TypeExpressionKind.Sum:
        self.sum.setScope(parent)
    of TypeExpressionKind.Distinct:
        self.base.setScope(parent)
    of TypeExpressionKind.Trait:
        self.trait.setScope(parent)
    of TypeExpressionKind.Expression:
        self.expression.setScope(parent)
proc setScope(self: TypeDef, parent: Scope) =
    if self.params.isSome:
        for iddef in self.params.get:
            iddef.setScope(parent)
    self.typ.setScope(parent)
proc setScope(self: Statement, parent: Scope) =
    case self.kind
    of StatementKind.Loop:
        self.`block`.setScope(parent)
    of StatementKind.For:
        self.pat.setScope(parent)
        self.val.setScope(parent)
        self.suite.setScope(parent)
    of StatementKind.While:
        self.branch.setScope(parent)
    of StatementKind.LetSection, StatementKind.VarSection, StatementKind.ConstSection:
        for iddef in self.iddefs:
            iddef.setScope(parent)
    of StatementKind.TypeSection:
        for iddef in self.typedefs:
            iddef.setScope(parent)
    of StatementKind.Asign:
        self.pat.setScope(parent)
        self.val.setScope(parent)
    of StatementKind.Funcdef:
        self.fn.setScope(parent)
    of StatementKind.Meta:
        self.meta.setScope(parent)
    of StatementKind.Discard:
        if self.`discard`.isSome:
            self.`discard`.get.setScope(parent)
    of StatementKind.Comments:
        discard
    of StatementKind.Expression:
        self.expression.setScope(parent)
    of StatementKind.Fail:
        discard

proc setScope*(self: Program, mainScope: Scope = newScope()): Scope {.discardable.} =
    self.scope = mainScope
    for s in self.stmts:
        s.setScope(mainScope)
    mainScope
