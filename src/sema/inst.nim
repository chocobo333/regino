
import sequtils
import options
import sugar

import scopes
import ../il


proc inst(self: Expression): Expression
proc inst(self: Suite): Suite
proc inst(self: Ident): Ident =
    newIdent(self.name, self.loc)
proc inst(self: Literal): Literal =
    case self.kind
    of LiteralKind.unit:
        Literal.unit()
    of LiteralKind.bool:
        Literal.boolean(self.boolval)
    of LiteralKind.integer:
        Literal.integer(self.intval, self.intbits)
    of LiteralKind.float:
        Literal.floating(self.floatval, self.floatbits)
    of LiteralKind.char:
        Literal.char(self.charval)
    of LiteralKind.string:
        Literal.str(self.strval)
    of LiteralKind.Univ:
        Literal.Univ(self.level)
proc inst(self: Pattern): Pattern =
    case self.kind
    of PatternKind.Literal:
        Pattern.literal(self.litval.inst)
    of PatternKind.Ident:
        Pattern.Id(self.ident.inst)
    of PatternKind.Tuple:
        Pattern.Tuple(self.patterns.map(inst))
    of PatternKind.Record:
        Pattern.Record(self.members.mapIt((it[0].inst, it[1].inst)))
    of PatternKind.UnderScore:
        Pattern.UnderScore()
proc inst(self: IdentDef): IdentDef =
    case self.kind:
    of DefKind.Def:
        IdentDef(
            kind: DefKind.Def,
            pat: self.pat.inst,
            typ: self.typ.map(inst),
            default: self.default.map(inst)
        )
    of DefKind.Comment:
        IdentDef(kind: DefKind.Comment, comment: self.comment)
proc inst(self: ElifBranch): ElifBranch =
    newElif(self.cond.inst, self.suite.inst)
proc inst(self: OfBranch): OfBranch =
    newOf(self.pat.inst, self.suite.inst)
proc inst(self: GenTypeDef): GenTypeDef =
    newGenTypedef(
        self.id.inst,
        self.ub.map(inst)
    )
proc inst(self: FunctionParam): FunctionParam =
    FunctionParam(
        implicit: @[],
        params: self.params.map(inst),
        rety: self.rety.map(inst)
    )
proc inst(self: Expression): Expression =
    case self.kind
    of ExpressionKind.Literal:
        Expression.literal(self.litval.inst)
    of ExpressionKind.Ident:
        Expression.Id(self.ident.inst)
    of ExpressionKind.Tuple:
        Expression.Tuple(self.exprs.map(inst))
    of ExpressionKind.Array:
        Expression.Array(self.exprs.map(inst))
    of ExpressionKind.Record:
        Expression.Record(self.members.mapIt((it[0].inst, it[1].inst)))
    of ExpressionKind.If:
        Expression.If(self.elifs.map(inst), self.elseb.map(inst))
    of ExpressionKind.When:
        Expression.When(self.elifs.map(inst), self.elseb.map(inst))
    of ExpressionKind.Case:
        Expression.Case(self.val.inst, self.ofs.map(inst), self.default.map(inst))
    of ExpressionKind.Call:
        Expression.Call(self.callee.inst, self.args.map(inst))
    of ExpressionKind.Command:
        Expression.Command(self.callee.inst, self.args.map(inst))
    of ExpressionKind.Dot:
        Expression.Dot(self.lhs.inst, self.rhs.inst)
    of ExpressionKind.Bracket:
        Expression.Bracket(self.callee.inst, self.args.map(inst))
    of ExpressionKind.Binary:
        Expression.Binary(self.op.inst, self.lhs.inst, self.rhs.inst)
    of ExpressionKind.Prefix:
        Expression.Prefix(self.op.inst, self.expression.inst)
    of ExpressionKind.Postfix:
        Expression.Postfix(self.op.inst, self.expression.inst)
    of ExpressionKind.Block:
        Expression.Block(self.body.inst)
    of ExpressionKind.Lambda:
        Expression.Lambda(self.param.inst, self.body.inst)
    of ExpressionKind.Malloc:
        Expression.Malloc(self.mtype.inst, self.msize.inst)
    of ExpressionKind.Typeof:
        Expression.Typeof(self.`typeof`.inst)
    of ExpressionKind.Ref:
        Expression.Ref(self.`ref`.inst)
    of ExpressionKind.FnType:
        Expression.FnType(self.args.map(inst), self.rety.inst)
    of ExpressionKind.Fail:
        Expression.Fail()
proc inst(self: Metadata): Metadata =
    case self.kind
    of MetadataKind.Link..MetadataKind.Subtype:
        Metadata(kind: self.kind, params: self.params.map(inst))
    of MetadataKind.Userdef:
        Metadata.Userdef(self.name, self.params.map(inst))
proc inst(fn: Function): Function =
    result = Function(
        isProp: fn.isProp,
        id: fn.id.inst,
        param: fn.param.inst,
        metadata: fn.metadata.map(inst),
        suite: fn.suite.map(inst)
    )
proc inst(self: SumConstructor): SumConstructor =
    case self.kind
    of SumConstructorKind.NoField:
        SumType.NoField(self.id.inst)
    of SumConstructorKind.UnnamedField:
        SumType.UnnamedField(self.id.inst, self.types.map(inst))
    of SumConstructorKind.NamedField:
        SumType.NamedField(self.id.inst, self.fields.mapIt((it[0].inst, it[1].inst)))
proc inst(self: SumType): SumType =
    newSumType(self.constructors.map(inst))
proc inst(self: TypeExpression): TypeExpression
proc inst(self: Trait): Trait =
    case self.kind
    of TraitKind.Is:
        Trait.Is(self.pat.inst, self.val.inst)
    of TraitKind.Func:
        Trait.Func(self.fn.inst)
proc inst(self: TraitType): TraitType =
    newTrait(self.pat.inst, self.typ.inst, self.traits.map(inst))
proc inst(self: TypeExpression): TypeExpression =
    case self.kind
    of TypeExpressionKind.Object:
        TypeExpression.Object(self.members.mapIt((it[0].inst, it[1].inst)))
    of TypeExpressionKind.Sum:
        TypeExpression.Sum(self.sum.inst)
    of TypeExpressionKind.Distinct:
        TypeExpression.Distinct(self.base.inst)
    of TypeExpressionKind.Trait:
        TypeExpression.TraitT(self.trait.inst)
    of TypeExpressionKind.Expression:
        TypeExpression.Expr(self.expression.inst)
proc inst(self: TypeDef): TypeDef =
    case self.kind
    of DefKind.Def:
        newTypedef(self.id.inst, self.params.map(it => it.map(inst)), self.typ.inst)
    of DefKind.Comment:
        newTypedef(self.comment)
proc inst(self: Statement): Statement =
    case self.kind
    of StatementKind.For:
        Statement.For(self.pat.inst, self.val.inst, self.suite.inst, self.loc)
    of StatementKind.While:
        Statement.While(self.branch.inst, self.loc)
    of StatementKind.Loop:
        Statement.Loop(self.`block`.inst, self.label.map(inst), self.loc)
    of StatementKind.LetSection:
        Statement.LetSection(self.iddefs.map(inst), self.loc)
    of StatementKind.VarSection:
        Statement.VarSection(self.iddefs.map(inst), self.loc)
    of StatementKind.ConstSection:
        Statement.ConstSection(self.iddefs.map(inst), self.loc)
    of StatementKind.TypeSection:
        Statement.TypeSection(self.typedefs.map(inst), self.loc)
    of StatementKind.Asign:
        Statement.Asign(self.pat.inst, self.op.inst, self.val.inst, self.loc)
    of StatementKind.Funcdef:
        Statement.Funcdef(self.fn.inst, self.loc)
    of StatementKind.Meta:
        Statement.Meta(self.meta.inst, self.loc)
    of StatementKind.Discard:
        Statement.Discard(self.`discard`.map(inst), self.loc)
    of StatementKind.Comments:
        Statement.Comments(self.comments, self.loc)
    of StatementKind.Expression:
        Statement.Expr(self.expression.inst)
    of StatementKind.Fail:
        Statement.Fail(self.loc)
proc inst(self: Suite): Suite =
    result = newSuite(self.stmts.map(inst))
    result.scope = newScope(self.scope.parent)
proc implInst*(fn: Function): Function =
    let scope = fn.param.scope.parent
    result = Function(
        isProp: fn.isProp,
        id: fn.id.inst,
        param: fn.param.inst,
        metadata: fn.metadata.map(inst),
        suite: fn.suite.map(inst)
    )
    result.setScope(scope)
