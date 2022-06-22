
import sequtils
import options
import sugar

import il

proc desugar(self: Expression): Expression
proc desugar(self: Suite): Suite
proc desugar(self: Ident): Ident =
    newIdent(self.name, self.loc)
proc desugar(self: Literal): Literal =
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
proc desugar(self: Pattern): Pattern =
    case self.kind
    of PatternKind.Literal:
        Pattern.literal(self.litval.desugar)
    of PatternKind.Ident:
        Pattern.Id(self.ident.desugar)
    of PatternKind.Tuple:
        Pattern.Tuple(self.patterns.map(desugar))
    of PatternKind.Record:
        Pattern.Record(self.members.mapIt((it[0].desugar, it[1].desugar)))
    of PatternKind.UnderScore:
        Pattern.UnderScore()
proc desugar(self: IdentDef): IdentDef =
    IdentDef(
        pat: self.pat.desugar,
        typ: self.typ.map(desugar),
        default: self.default.map(desugar)
    )
proc desugar(self: ElifBranch): ElifBranch =
    newElif(self.cond.desugar, self.suite.desugar)
proc desugar(self: OfBranch): OfBranch =
    newOf(self.pat.desugar, self.suite.desugar)
proc desugar(self: GenTypeDef): GenTypeDef =
    newGenTypedef(
        self.id.desugar,
        self.ub.map(desugar)
    )
proc desugar(self: FunctionParam): FunctionParam =
    FunctionParam(
        implicit: @[],
        params: self.params.map(desugar),
        rety: self.rety.map(desugar)
    )
proc desugar(self: Expression): Expression =
    proc makePrimitiveProc(callee: Expression, args: seq[Expression]): Option[Expression] = 
        if callee.kind != ExpressionKind.Ident:
            return none(Expression)
        case callee.ident.name:
        of PrimitiveKeyWord.PKTypeof:
            some(Expression.Typeof(args[0].desugar))
        of PrimitiveKeyWord.PKMalloc:
            some(Expression.Malloc(args[0].desugar, args[1].desugar))
        of PrimitiveKeyWord.PKRealloc:
            some(Expression.Realloc(args[0].desugar, args[1].desugar))
        of PrimitiveKeyWord.PKPtrSet:
            some(Expression.Ptrset(args[0].desugar, args[1].desugar, args[2].desugar))
        of PrimitiveKeyWord.PKPtrGet:
            some(Expression.Ptrget(args[0].desugar, args[1].desugar))
        else:
            none(Expression)
    case self.kind
    of ExpressionKind.Literal:
        Expression.literal(self.litval.desugar)
    of ExpressionKind.Ident:
        Expression.Id(self.ident.desugar)
    of ExpressionKind.Tuple:
        Expression.Tuple(self.exprs.map(desugar))
    of ExpressionKind.Array:
        Expression.Array(self.exprs.map(desugar))
    of ExpressionKind.Record:
        Expression.Record(self.members.mapIt((it[0].desugar, it[1].desugar)))
    of ExpressionKind.ObjCons:
        Expression.ObjCons(self.typname.desugar, self.members.mapIt((it[0].desugar, it[1].desugar)))
    of ExpressionKind.If:
        Expression.If(self.elifs.map(desugar), self.elseb.map(desugar))
    of ExpressionKind.When:
        Expression.When(self.elifs.map(desugar), self.elseb.map(desugar))
    of ExpressionKind.Case:
        Expression.Case(self.val.desugar, self.ofs.map(desugar), self.default.map(desugar))
    of ExpressionKind.Call:
        let desugared = makePrimitiveProc(self.callee, self.args)
        if desugared.isSome: desugared.get else: Expression.Call(self.callee.desugar, self.args.map(desugar))
    of ExpressionKind.Command:
        let desugared = makePrimitiveProc(self.callee, self.args)
        if desugared.isSome: desugared.get else: Expression.Command(self.callee.desugar, self.args.map(desugar))
    of ExpressionKind.Dot:
        let desugared = makePrimitiveProc(self.rhs, @[self.lhs] & self.dotArgs)
        if desugared.isSome: desugared.get else: Expression.Dot(self.lhs.desugar, self.rhs.desugar)
    of ExpressionKind.Bracket:
        Expression.Bracket(self.callee.desugar, self.args.map(desugar))
    of ExpressionKind.Binary:
        Expression.Binary(self.op.desugar, self.lhs.desugar, self.rhs.desugar)
    of ExpressionKind.Prefix:
        Expression.Prefix(self.op.desugar, self.expression.desugar)
    of ExpressionKind.Postfix:
        Expression.Postfix(self.op.desugar, self.expression.desugar)
    of ExpressionKind.Block:
        Expression.Block(self.body.desugar)
    of ExpressionKind.Lambda:
        Expression.Lambda(self.param.desugar, self.body.desugar)
    of ExpressionKind.Malloc:
        Expression.Malloc(self.mtype.desugar, self.msize.desugar)
    of ExpressionKind.Realloc:
        Expression.Realloc(self.rptr.desugar, self.msize.desugar)
    of ExpressionKind.Ptrset:
        Expression.Ptrset(self.`ptr`.desugar, self.idx.desugar, self.v.desugar)
    of ExpressionKind.Ptrget:
        Expression.Ptrget(self.`ptr`.desugar, self.idx.desugar)
    of ExpressionKind.Typeof:
        Expression.Typeof(self.`typeof`.desugar)
    of ExpressionKind.Ref:
        Expression.Ref(self.`ref`.desugar)
    of ExpressionKind.FnType:
        Expression.FnType(self.args.map(desugar), self.rety.desugar)
    of ExpressionKind.IntCast:
        Expression.IntCast(self.int_exp, self.from, self.to)
    of ExpressionKind.Fail:
        Expression.Fail()
proc desugar(self: Metadata): Metadata =
    case self.kind
    of MetadataKind.Link..MetadataKind.Subtype:
        Metadata(kind: self.kind, params: self.params.map(desugar))
    of MetadataKind.Userdef:
        Metadata.Userdef(self.name, self.params.map(desugar))
proc desugar(fn: Function): Function =
    result = Function(
        isProp: fn.isProp,
        id: fn.id.desugar,
        param: fn.param.desugar,
        metadata: fn.metadata.map(desugar),
        suite: fn.suite.map(desugar)
    )
proc desugar(self: SumConstructor): SumConstructor =
    case self.kind
    of SumConstructorKind.NoField:
        SumType.NoField(self.id.desugar)
    of SumConstructorKind.UnnamedField:
        SumType.UnnamedField(self.id.desugar, self.types.map(desugar))
    of SumConstructorKind.NamedField:
        SumType.NamedField(self.id.desugar, self.fields.mapIt((it[0].desugar, it[1].desugar)))
proc desugar(self: SumType): SumType =
    newSumType(self.constructors.map(desugar))
proc desugar(self: TypeExpression): TypeExpression
proc desugar(self: Trait): Trait =
    case self.kind
    of TraitKind.Is:
        Trait.Is(self.pat.desugar, self.val.desugar)
    of TraitKind.Func:
        Trait.Func(self.fn.desugar)
proc desugar(self: TraitType): TraitType =
    newTrait(self.pat.desugar, self.typ.desugar, self.traits.map(desugar))
proc desugar(self: TypeExpression): TypeExpression =
    case self.kind
    of TypeExpressionKind.Object:
        TypeExpression.Object(self.members.mapIt((it[0].desugar, it[1].desugar)))
    of TypeExpressionKind.Sum:
        TypeExpression.Sum(self.sum.desugar)
    of TypeExpressionKind.Distinct:
        TypeExpression.Distinct(self.base.desugar)
    of TypeExpressionKind.Trait:
        TypeExpression.TraitT(self.trait.desugar)
    of TypeExpressionKind.Expression:
        TypeExpression.Expr(self.expression.desugar)
proc desugar(self: TypeDef): TypeDef =
    newTypedef(self.id.desugar, self.params.map(it => it.map(desugar)), self.typ.desugar, self.comments)
proc desugar(self: IdentDefSection): IdentDefSection = 
    newIddefSection(self.iddefs.map(desugar), self.comments)
proc desugar(self: TypeDefSection): TypeDefSection = 
    newTypedefSection(self.typedefs.map(desugar), self.comments)
proc desugar(self: Statement): Statement =
    case self.kind
    of StatementKind.For:
        Statement.For(self.pat.desugar, self.val.desugar, self.suite.desugar, self.loc)
    of StatementKind.While:
        Statement.While(self.branch.desugar, self.loc)
    of StatementKind.Loop:
        Statement.Loop(self.`block`.desugar, self.label.map(desugar), self.loc)
    of StatementKind.LetSection:
        Statement.LetSection(self.iddefSection.desugar, self.loc)
    of StatementKind.VarSection:
        Statement.VarSection(self.iddefSection.desugar, self.loc)
    of StatementKind.ConstSection:
        Statement.ConstSection(self.iddefSection.desugar, self.loc)
    of StatementKind.TypeSection:
        Statement.TypeSection(self.typedefSection.desugar, self.loc)
    of StatementKind.Asign:
        Statement.Asign(self.pat.desugar, self.op.desugar, self.val.desugar, self.loc)
    of StatementKind.Funcdef:
        Statement.Funcdef(self.fn.desugar, self.loc)
    of StatementKind.Meta:
        Statement.Meta(self.meta.desugar, self.loc)
    of StatementKind.Discard:
        Statement.Discard(self.`discard`.map(desugar), self.loc)
    of StatementKind.Comments:
        Statement.Comments(self.comments, self.loc)
    of StatementKind.Expression:
        Statement.Expr(self.expression.desugar)
    of StatementKind.Fail:
        Statement.Fail(self.loc)
proc desugar(self: Suite): Suite =
    newSuite(self.stmts.map(desugar))
proc desugar*(self: Program): Program = 
    Program(stmts: self.stmts.map(desugar), scope: self.scope)