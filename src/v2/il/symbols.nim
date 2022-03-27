
import options

import il


proc Var*(_: typedesc[Symbol], decl: Ident, typ: Value, impl: IdentDef, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Var, decl: decl, typ: typ, impl_iddef: impl, global: global)
    typ.symbol = some result
proc Let*(_: typedesc[Symbol], decl: Ident, typ: Value, impl: IdentDef, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Let, decl: decl, typ: typ, impl_iddef: impl, global: global)
    typ.symbol = some result
proc Const*(_: typedesc[Symbol], decl: Ident, typ: Value, impl: IdentDef, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Const, decl: decl, typ: typ, impl_iddef: impl, global: global)
    typ.symbol = some result
proc Typ*(_: typedesc[Symbol], decl: Ident, val: Value, impl: TypeDef, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Typ, decl: decl, val: val, typ: val.typ, impl_typedef: impl, global: global)
    val.symbol = some result
proc Func*(_: typedesc[Symbol], decl: Ident, typ: Value, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Func, decl: decl, typ: typ, global: global)
    typ.symbol = some result

proc empty*(_: typedesc[Symbol]): Symbol = Symbol()
