
import il


proc Is*(_: typedesc[Trait], pat: Pattern, val: TypeExpression): Trait =
    Trait(kind: TraitKind.Is, pat: pat, val: val)
proc Func*(_: typedesc[Trait], fn: Function): Trait =
    Trait(kind: TraitKind.Func, fn: fn)
proc newTrait*(pat: Pattern, typ: Expression, traits: seq[Trait]): TraitType =
    TraitType(pat: pat, typ: typ, traits: traits)
