
import tables
import ir


proc Bottom*(_: typedesc[Type]): Type =
    Type(kind: TypeKind.Bottom)
proc Unit*(_: typedesc[Type]): Type =
    Type(kind: TypeKind.Unit)
proc Univ*(_: typedesc[Type], level: uint): Type =
    Type(kind: TypeKind.Univ, level: level)
proc Value*(_: typedesc[Type], val: Value): Type =
    Type(kind: TypeKind.Value, val: val)
proc Integer*(_: typedesc[Type], nbits: uint): Type =
    Type(kind: TypeKind.Integer, nbits: nbits)
proc Float*(_: typedesc[Type], nbits: uint): Type =
    Type(kind: TypeKind.Float, nbits: nbits)
proc Char*(_: typedesc[Type]): Type =
    Type(kind: TypeKind.Char)
proc CString*(_: typedesc[Type]): Type =
    Type(kind: TypeKind.CString)
proc Pair*(_: typedesc[Type], first, second: Type): Type =
    Type(kind: TypeKind.Pair, first: first, second: second)
proc Record*(_: typedesc[Type], members: Table[string, Type]): Type =
    Type(kind: TypeKind.Record, members: members)
proc Object*(_: typedesc[Type], members: Table[string, Type]): Type =
    Type(kind: TypeKind.Object, members: members)
proc Arrow*(_: typedesc[Type], params: seq[Type], rety: Type): Type =
    Type(kind: TypeKind.Arrow, params: params, rety: rety)
proc Cons*(_: typedesc[Type], cons: PiType, args: seq[Type]): Type =
    Type(kind: TypeKind.Cons, cons: cons, args: args)
proc Trait*(_: typedesc[Type], paty: (Pattern, Type), iss: seq[(Pattern, Value)], fns: seq[Function], fnss: seq[FunctionSignature]): Type =
    Type(kind: TypeKind.Trait, paty: paty, iss: iss, fns: fns, fnss: fnss)
# proc Var*(_: typedesc[Type]): Type =
#     Type(kind: TypeKind.Var)
proc Gen*(_: typedesc[Type], gt: GenericType): Type =
    Type(kind: TypeKind.Gen, gt: gt)
proc Link*(_: typedesc[Type], to: Type): Type =
    Type(kind: TypeKind.Link, to: to)
