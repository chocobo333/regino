
import tables
import options
import ../../lineinfos

import ir

proc newIdent*(name: string, loc: Location): Ident =
    Ident(name: name, loc: loc)

proc Unit*(_: typedesc[Value]): Value =
    Value(kind: ValueKind.Unit)
proc Univ*(_: typedesc[Value], level: uint): Value =
    Value(kind: ValueKind.Univ, level: level)
proc Bool*(_: typedesc[Value], boolval: bool): Value =
    Value(kind: ValueKind.Bool, boolval: boolval)
proc Integer*(_: typedesc[Value], intval: BiggestInt, intbits: uint): Value =
    Value(kind: ValueKind.Integer, intval: intval, intbits: intbits)
proc Float*(_: typedesc[Value], floatval: BiggestFloat, floatbits: uint): Value =
    Value(kind: ValueKind.Float, floatval: floatval, floatbits: floatbits)
proc Char*(_: typedesc[Value], charval: char): Value =
    Value(kind: ValueKind.Char, charval: charval)
proc CString*(_: typedesc[Value], strval: string): Value =
    Value(kind: ValueKind.CString, strval: strval)
proc Array*(_: typedesc[Value], vals: seq[Type]): Value =
    Value(kind: ValueKind.Array, vals: vals)
proc Function*(_: typedesc[Value], fn: Function): Value =
    Value(kind: ValueKind.Function, fn: fn)

proc Bottom*(_: typedesc[Type]): Type =
    Type(kind: TypeKind.Bottom)
proc Unit*(_: typedesc[Type]): Type =
    Type(kind: TypeKind.Unit)
proc Univ*(_: typedesc[Type], level: uint): Type =
    Type(kind: TypeKind.Univ, level: level)
proc value*(_: typedesc[Type], val: Value): Type =
    Type(kind: TypeKind.Value, val: val)
proc Bool*(_: typedesc[Type]): Type =
    Type(kind: TypeKind.Bool)
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
proc Array*(_: typedesc[Type], length: uint, base: Type): Type =
    Type(kind: TypeKind.Array, length: length, base: base)
proc Distinct*(_: typedesc[Type], base: Type): Type =
    Type(kind: TypeKind.Distinct, base: base)
proc Singleton*(_: typedesc[Type], base: Type): Type =
    Type(kind: TypeKind.Singleton, base: base)
proc Ptr*(_: typedesc[Type], pointee: Type): Type =
    Type(kind: TypeKind.Ptr, pointee: pointee)
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

proc SubType*(_: typedesc[Metadata]): Metadata =
    Metadata(kind: MetadataKind.SubType)

proc Ref*(_: typedesc[TypeExpression], to: TypeExpression): TypeExpression =
    TypeExpression(kind: TypeExpressionKind.Ref, to: to)
proc Object*(_: typedesc[TypeExpression], ident: Ident, members: Table[Ident, Expression]): TypeExpression =
    TypeExpression(kind: TypeExpressionKind.Object, ident: ident, members: members)
proc Variant*(_: typedesc[TypeExpression], elements: seq[VariantElement]): TypeExpression =
    TypeExpression(kind: TypeExpressionKind.Variant, elements: elements)
proc Trait*(_: typedesc[TypeExpression], paty: (Pattern, Expression), iss: seq[(Pattern, Expression)], fns: seq[Function], fnss: seq[FunctionSignature]): TypeExpression =
    TypeExpression(kind: TypeExpressionKind.Trait, paty: paty, iss: iss, fns: fns, fnss: fnss)
proc Expr*(_: typedesc[TypeExpression], expression: Expression): TypeExpression =
    TypeExpression(kind: TypeExpressionKind.Expression, expression: expression)

proc NoField*(_: typedesc[VariantElement]): VariantElement =
    VariantElement(kind: VariantElementKind.NoField)
proc Tuple*(_: typedesc[VariantElement], fields: seq[Expression]): VariantElement =
    VariantElement(kind: VariantElementKind.Tuple, fields: fields)
proc Object*(_: typedesc[VariantElement], members: Table[Ident, ir.Expression]): VariantElement =
    VariantElement(kind: VariantElementKind.Object, members: members)

proc Lit*(_: typedesc[Expression], litval: Literal, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Literal, litval: litval, loc: loc)
proc Id*(_: typedesc[Expression], ident: Ident, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Ident, ident: ident, loc: loc)
proc Call*(_: typedesc[Expression], callee: Expression, args: seq[Expression], loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Call, callee: callee, args: args, loc: loc)
proc Apply*(_: typedesc[Expression], callee: Expression, args: seq[Expression], loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Apply, callee: callee, args: args, loc: loc)
proc If*(_: typedesc[Expression], cond: Expression, then: Expression, els: Expression, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.If, cond: cond, then: then, els: els, loc: loc)
proc Case*(_: typedesc[Expression], ofs: (Pattern, Expression), loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Case, ofs: ofs, loc: loc)
proc Pair*(_: typedesc[Expression], first: Expression, second: Expression, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Pair, first: first, second: second, loc: loc)
proc Array*(_: typedesc[Expression], elements: seq[Expression], loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Array, elements: elements, loc: loc)
proc Record*(_: typedesc[Expression], obj: Expression, implicits: seq[Expression], members: Table[ir.Ident, ir.Expression], loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Record, obj: obj, implicits: implicits, members: members, loc: loc)
proc ObjCons*(_: typedesc[Expression], obj: Expression, implicits: seq[Expression], members: Table[ir.Ident, ir.Expression], loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.ObjCons, obj: obj, implicits: implicits, members: members, loc: loc)
proc Ref*(_: typedesc[Expression], to: Expression, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Ref, to: to, loc: loc)
proc Import*(_: typedesc[Expression], module: Ident, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Import, module: module, loc: loc)
proc LetSection*(_: typedesc[Expression], iddefs: seq[IdentDef], loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.LetSection, iddefs: iddefs, loc: loc)
proc VarSection*(_: typedesc[Expression], iddefs: seq[IdentDef], loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.VarSection, iddefs: iddefs, loc: loc)
proc TypeSection*(_: typedesc[Expression], typedefs: seq[TypeDef], loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.TypeSection, typedefs: typedefs, loc: loc)
proc Assign*(_: typedesc[Expression], assign_lval: Pattern, assign_val: Expression, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Assign, assign_lval: assign_lval, assign_val: assign_val, loc: loc)
proc Funcdef*(_: typedesc[Expression], fn: Function, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Funcdef, fn: fn, loc: loc)
proc ImportLL*(_: typedesc[Expression], signature: FunctionSignature, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.ImportLL, signature: signature, loc: loc)
proc Loop*(_: typedesc[Expression], lable: Ident, `block`: Expression, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Loop, lable: lable, `block`: `block`, loc: loc)
proc Discard*(_: typedesc[Expression], lable: Ident, `block`: Expression, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Discard, lable: lable, `block`: `block`, loc: loc)
proc Seq*(_: typedesc[Expression], expressions: seq[Expression], scope: Scope, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Seq, expressions: expressions, scope: scope, loc: loc)
proc Typeof*(_: typedesc[Expression], `typeof`: Expression, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Typeof, `typeof`: `typeof`, loc: loc)
proc Malloc*(_: typedesc[Expression], mtype, msize: Expression, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Malloc, mtype: mtype, msize: msize, loc: loc)
proc Realloc*(_: typedesc[Expression], rptr, msize: Expression, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.Realloc, rptr: rptr, msize: msize, loc: loc)
proc PtrSet*(_: typedesc[Expression], `ptr`, index, val: Expression, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.PtrSet, `ptr`: `ptr`, index: index, val: val, loc: loc)
proc PtrGet*(_: typedesc[Expression], `ptr`, index: Expression, loc: Location): Expression =
    ir.Expression(kind: ExpressionKind.PtrGet, `ptr`: `ptr`, index: index, loc: loc)

proc Unit*(_: typedesc[Literal]): Literal =
    Literal(kind: LiteralKind.Unit)
proc Bool*(_: typedesc[Literal], boolval: bool): Literal =
    Literal(kind: LiteralKind.Bool, boolval: bool)
proc Integer*(_: typedesc[Literal], intval: BiggestInt, intbits: uint): Literal =
    Literal(kind: LiteralKind.Integer, intval: intval, intbits: intbits)
proc Float*(_: typedesc[Literal], floatval: BiggestFloat, floatbits: uint): Literal =
    Literal(kind: LiteralKind.Float, floatval: floatval, floatbits: floatbits)
proc Char*(_: typedesc[Literal], charval: string): Literal =
    Literal(kind: LiteralKind.Char, charval: charval)
proc CString*(_: typedesc[Literal], strval: string): Literal =
    Literal(kind: LiteralKind.CString, strval: strval)

proc Lit*(_: typedesc[Pattern], litval: Literal): Pattern =
    Pattern(kind: PatternKind.Literal, litval: litval)
proc Id*(_: typedesc[Pattern], ident: Ident): Pattern =
    Pattern(kind: PatternKind.Ident, ident: ident)
proc Tuple*(_: typedesc[Pattern], tag: Option[ir.Ident], patterns: seq[Pattern]): Pattern =
    Pattern(kind: PatternKind.Tuple, tag: tag, patterns: patterns)
proc Record*(_: typedesc[Pattern], tag: Option[ir.Ident], members: seq[(Ident, Pattern)]): Pattern =
    Pattern(kind: PatternKind.Record, tag: tag, members: members)


import sequtils

import macros
import ast_pattern_matching


proc parseImpl(body: NimNode): NimNode =
    discard
macro parse(body: untyped): untyped =
    body.parseImpl



when isMainModule:
    import tos

    let program = parse:
        type
            int = typeof(0)
        proc id(a: int): int =
            a
        0
    echo program
