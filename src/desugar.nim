import sequtils
import options
import sugar

import il

proc desugar(self: Suite): Suite

proc desugar(self: Ident): Ident = self

proc desugar(self: Expression): Expression = 
    case self.kind:
    of ExpressionKind.Call:
        if self.callee.kind == ExpressionKind.Ident:
            let callee = self.callee.ident.name
            case callee:
            # of "malloc":
            #     Expression.Malloc(self.args[0], self.args[1], self.loc)
            of "typeof":
                Expression.Typeof(self.args[0], self.loc)
            else:
                self
        else:
            self
    else:
        self

proc desugar(self: TypeExpression): TypeExpression = 
    case self.kind:
    of TypeExpressionKind.Expression:
        TypeExpression.Expr(self.expression.desugar)
    else:
        self

proc desugar(self: GenTypeDef): GenTypeDef = 
    newGenTypedef(self.id.desugar, self.ub.map(desugar))
proc desugar(self: TypeDef): TypeDef = 
    newTypedef(
        self.id.desugar,
        self.params.map(it => it.map(desugar)),
        self.typ.desugar,
        self.comments
    )

proc desugar(self: TypeDefSection): TypeDefSection = 
    TypeDefSection(typedefs: self.typedefs.map(desugar), comments: self.comments)

proc desugar(self: Statement): Statement =
    case self.kind:
    of StatementKind.Expression:
        Statement.Expr(self.expression.desugar)
    of StatementKind.TypeSection:
        Statement.TypeSection(self.typedefSection.desugar, self.loc)
    else:
        self

proc desugar(self: Suite): Suite = 
    Suite(stmts: self.stmts.map(desugar), scope: self.scope)

proc desugar*(self: Program): Program = 
    Program(stmts: self.stmts.map(desugar), scope: self.scope)
    