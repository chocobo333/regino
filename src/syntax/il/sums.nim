
import il


proc NoField*(_: typedesc[SumType], id: Ident): SumConstructor =
    SumConstructor(kind: SumConstructorKind.NoField, id: id)
proc UnnamedField*(_: typedesc[SumType], id: Ident, types: seq[Expression]): SumConstructor =
    SumConstructor(kind: SumConstructorKind.UnnamedField, id: id, types: types)
proc NamedField*(_: typedesc[SumType], id: Ident, fields: seq[(Ident, Expression)]): SumConstructor =
    SumConstructor(kind: SumConstructorKind.NamedField, id: id, fields: fields)
proc newSumType*(cons: seq[SumConstructor]): SumType =
    SumType(constructors: cons)
