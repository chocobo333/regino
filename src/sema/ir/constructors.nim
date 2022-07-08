
import tables
import options

import ir


proc Unit*(_: typedesc[Value]): Value =
    Value(kind: ValueKind.Unit)
proc Univ*(_: typedesc[Value], level: uint): Value =
    Value(kind: ValueKind.Univ, level: level)
proc Integer*(_: typedesc[Value], intval: BiggestInt, intbits: uint): Value =
    Value(kind: ValueKind.Integer, intval: intval, intbits: intbits)
proc Float*(_: typedesc[Value], floatval: BiggestFloat, floatbits: uint): Value =
    Value(kind: ValueKind.Float, floatval: floatval, floatbits: floatbits)
proc Char*(_: typedesc[Value], charval: char): Value =
    Value(kind: ValueKind.Char, charval: charval)
proc CString*(_: typedesc[Value], strval: string): Value =
    Value(kind: ValueKind.CString, strval: strval)
proc Function*(_: typedesc[Value], fn: Function): Value =
    Value(kind: ValueKind.Function, fn: fn)

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
proc Recursive*(_: typedesc[Type], self: VarId, body: Type): Type =
    Type(kind: TypeKind.Recursive, self: self, body: body)
proc Trait*(_: typedesc[Type], paty: (Pattern, Type), iss: seq[(Pattern, Value)], fns: seq[Function], fnss: seq[FunctionSignature]): Type =
    Type(kind: TypeKind.Trait, paty: paty, iss: iss, fns: fns, fnss: fnss)
proc Var*(_: typedesc[Type], tv: TypeVar): Type =
    Type(kind: TypeKind.Var, tv: tv)
proc Gen*(_: typedesc[Type], gt: GenericType): Type =
    Type(kind: TypeKind.Gen, gt: gt)
proc Link*(_: typedesc[Type], to: Type): Type =
    Type(kind: TypeKind.Link, to: to)

proc NotDeclared*(_: typedesc[Symbol], ident: Ident, typ: Type, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.NotDeclared, ident: ident, typ: typ, global: global)
    typ.symbol = some result
proc Var*(_: typedesc[Symbol], ident: Ident, typ: Type, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Var, ident: ident, typ: typ, global: global)
    typ.symbol = some result
proc Let*(_: typedesc[Symbol], ident: Ident, typ: Type, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Let, ident: ident, typ: typ, global: global)
    typ.symbol = some result
proc Const*(_: typedesc[Symbol], ident: Ident, typ: Type, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Const, ident: ident, typ: typ, global: global)
    typ.symbol = some result
proc Param*(_: typedesc[Symbol], ident: Ident, typ: Type, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Param, ident: ident, typ: typ, global: global)
    ident.typ = typ
    typ.symbol = some result
proc Typ*(_: typedesc[Symbol], ident: Ident, val: Type, global: bool): Symbol =
    let typ = val.typ
    ident.typ = typ
    result = Symbol(kind: SymbolKind.Typ, ident: ident, val: val, typ: typ, global: global)
    typ.symbol = some result
proc GenParam*(_: typedesc[Symbol], ident: Ident, val: Type): Symbol =
    let typ = val.typ
    result = Symbol(kind: SymbolKind.GenParam, ident: ident, val: val, typ: typ, global: false)
    typ.symbol = some result
proc Func*(_: typedesc[Symbol], ident: Ident, typ: Type, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Func, ident: ident, typ: typ, global: global)
    typ.symbol = some result
proc Field*(_: typedesc[Symbol], ident: Ident, typ: Type, index: int,  global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Field, ident: ident, typ: typ, index: index, global: global)
    typ.symbol = some result
# proc Enum*(_: typedesc[Symbol], ident: Ident, typ: Value, decl: SumConstructor, global: bool): Symbol =
#     result = Symbol(kind: SymbolKind.Enum, ident: ident, typ: typ, enumdef: decl, global: global)
#     typ.symbol = some result
