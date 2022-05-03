
import options

import il


proc Var*(_: typedesc[Symbol], id: Ident, typ: Value, decl: IdentDef, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Var, id: id, typ: typ, decl_iddef: decl, global: global)
    typ.symbol = some result
proc Let*(_: typedesc[Symbol], id: Ident, typ: Value, decl: IdentDef, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Let, id: id, typ: typ, decl_iddef: decl, global: global)
    typ.symbol = some result
proc Const*(_: typedesc[Symbol], id: Ident, typ: Value, decl: IdentDef, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Const, id: id, typ: typ, decl_iddef: decl, global: global)
    typ.symbol = some result
proc Param*(_: typedesc[Symbol], id: Ident, typ: Value, decl: IdentDef, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Param, id: id, typ: typ, decl_iddef: decl, global: global)
    id.typ = typ
    typ.symbol = some result
proc Typ*(_: typedesc[Symbol], id: Ident, val: Value, decl: TypeDef, global: bool): Symbol =
    let typ = val.typ
    id.typ = typ
    val.ident = some id
    result = Symbol(kind: SymbolKind.Typ, id: id, val: val, typ: typ, decl_typedef: decl, global: global)
    typ.symbol = some result
proc GenParam*(_: typedesc[Symbol], id: Ident, val: Value, decl: GenTypeDef): Symbol =
    let typ = val.typ
    result = Symbol(kind: SymbolKind.GenParam, id: id, val: val, typ: typ, decl_gendef: decl, global: false)
    typ.symbol = some result
proc Func*(_: typedesc[Symbol], id: Ident, typ: Value, decl: Function, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Func, id: id, typ: typ, decl_funcdef: decl, global: global)
    typ.symbol = some result

proc empty*(_: typedesc[Symbol]): Symbol = Symbol()

import ../lineinfos
proc loc*(self: Symbol): Location =
    self.id.loc
