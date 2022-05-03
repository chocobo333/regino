
import il

proc Object*(_: typedesc[TypeExpression], members: seq[(Ident, TypeExpression)]): TypeExpression =
    TypeExpression(kind: TypeExpressionKind.Object, members: members)
proc Sum*(_: typedesc[TypeExpression], sum: SumType): TypeExpression =
    TypeExpression(kind: TypeExpressionKind.Sum, sum: sum)
proc Distinct*(_: typedesc[TypeExpression], base: TypeExpression): TypeExpression =
    TypeExpression(kind: TypeExpressionKind.Distinct, base: base)
proc TraitT*(_: typedesc[TypeExpression], trait: TraitType): TypeExpression =
    TypeExpression(kind: TypeExpressionKind.Trait, trait: trait)
proc Expr*(_: typedesc[TypeExpression], exp: Expression): TypeExpression =
    TypeExpression(kind: TypeExpressionKind.Expression, expression: exp)
