
import sequtils
import tables
import options
import sugar

import syntax/il
import syntax/projects as ilprojects
import sema/projects as irprojects
import sema/ir/[
    ir, 
    constructors
]

import lineinfos

let 
    dummyExpression = ir.Expression.Lit(ir.Literal.Unit, newLocation())
    dummyTypeExpression = ir.TypeExpression.Expr(dummyExpression)

proc il2ir*(self: il.Ident, scope: Scope): ir.Ident =
    constructors.newIdent(self.name, self.loc)

proc il2ir*(self: il.Literal, scope: Scope): ir.Literal =
    case self.kind:
    of il.LiteralKind.unit:
        ir.Literal.Unit
    of il.LiteralKind.bool:
        ir.Literal.Bool(self.boolval)
    of il.LiteralKind.integer:
        ir.Literal.Integer(self.intval, self.intbits)
    of il.LiteralKind.float:
        ir.Literal.Float(self.floatval, self.floatbits)
    of il.LiteralKind.char:
        ir.Literal.Char(self.charval)
    of il.LiteralKind.string:
        ir.Literal.CString(self.strval)
    of il.LiteralKind.Univ:
        ir.Literal.Univ(self.level)

proc il2ir*(self: il.Expression, scope: Scope): ir.Expression =
    # TODO:
    result = case self.kind:
    of il.ExpressionKind.Literal:
        ir.Expression.Lit(self.litval.il2ir(scope), self.loc)
    of il.ExpressionKind.Ident:
        ir.Expression.Id(self.ident.il2ir(scope), self.loc)
    of il.ExpressionKind.Array:
        ir.Expression.Array(self.exprs.map(it => it.il2ir(scope)), self.loc)
    else:
        assert(false, "Not Implemented")
        dummyExpression

    result.scope = scope

proc il2ir*(self: il.TypeExpression, scope: Scope): ir.TypeExpression =
    # TODO:
    result = case self.kind:
    of il.TypeExpressionKind.Expression:
        ir.TypeExpression.Expr(self.expression.il2ir(scope))
    else:
        assert(false, "Not Implemented")
        dummyTypeExpression

    if self.isRef:
        result = ir.TypeExpression.Ref(result)


proc il2ir*(self: il.Pattern, scope: Scope): ir.Pattern =
    case self.kind:
    of il.PatternKind.Literal:
        ir.Pattern.Lit(self.litval.il2ir(scope))
    of il.PatternKind.Ident:
        ir.Pattern.Id(self.ident.il2ir(scope))
    of il.PatternKind.Tuple:
        ir.Pattern.Tuple(self.tag.map(it => it.il2ir(scope)), self.patterns.map(it => it.il2ir(scope)))
    of il.PatternKind.Record:
        ir.Pattern.Record(self.tag.map(it => it.il2ir(scope)), self.members.map(_ => (il2ir(_[0], scope), il2ir(_[1], scope))))
    of il.PatternKind.UnderScore:
        let ident = constructors.newIdent("_", self.loc)
        ir.Pattern.Id(ident)

proc il2ir*(self: il.IdentDef, scope: Scope): ir.IdentDef =
    ir.IdentDef(
        pat: self.pat.il2ir(scope), 
        typ: self.typ.map(it => it.il2ir(scope)), 
        default: self.default.map(it => it.il2ir(scope)),
        loc: self.loc
    )

proc il2ir*(self: il.GenTypeDef, scope: Scope): ir.GenTypeDef =
    ir.GenTypeDef(
        ident: self.id.il2ir(scope),
        typ: self.typ.map(it => it.il2ir(scope)),
        ub: self.ub.map(it => it.il2ir(scope)),
        loc: self.loc
    )

proc il2ir*(self: il.TypeDef, scope: Scope): ir.Expression =
    let 
        scope = newScope(scope)
        params = if self.params.isSome: self.params.get.map(it => it.il2ir(scope)) else: @[]
        typeDef = ir.TypeDef(
            ident: self.id.il2ir(scope),
            params: params,
            typ: self.typ.il2ir(scope),
            loc: self.loc
        )
    result = ir.Expression.TypeSection(typeDef, self.loc)
    result.scope = scope

proc il2ir*(self: IdentDefSection, scope: Scope): seq[ir.IdentDef] =
    self.iddefs.map(it => it.il2ir(scope))

proc il2ir*(self: TypeDefSection, scope: Scope): seq[ir.Expression] =
    self.typedefs.map(it => it.il2ir(scope))

proc il2ir*(self: Statement, scope: Scope): ir.Expression =
    # TODO:
    result = case self.kind:
    of StatementKind.LetSection:
        ir.Expression.LetSection(self.iddefSection.il2ir(scope), self.loc)
    of StatementKind.VarSection:
        ir.Expression.VarSection(self.iddefSection.il2ir(scope), self.loc)
    of StatementKind.ConstSection:
        ir.Expression.ConsSection(self.iddefSection.il2ir(scope), self.loc)
    of StatementKind.TypeSection:
        let typeDefs = self.typedefSection.il2ir(scope)
        ir.Expression.Seq(typeDefs, scope, self.loc)
    of StatementKind.Funcdef:
        dummyExpression
    of StatementKind.Expression:
        self.expression.il2ir(scope)
    else:
        dummyExpression

    result.scope = scope

proc il2ir*(self: Program): ir.Expression =
    let scope = newScope()
    ir.Expression.Seq(
        self.stmts.map(it => it.il2ir(scope)), 
        scope,
        # TODO:
        # calculate location from stmts or 
        # change definition of Program to have location and revise parser of Program
        newLocation()
    )

proc il2ir*(self: ilprojects.Project): irprojects.Project =
    result = irprojects.newProject(self.main)
    for uri, program in self.program.pairs:
        result.programs[uri] = program.il2ir