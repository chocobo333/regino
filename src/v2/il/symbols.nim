
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
    typ.symbol = some result
proc Typ*(_: typedesc[Symbol], id: Ident, val: Value, decl: TypeDef, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Typ, id: id, val: val, typ: val.typ, decl_typedef: decl, global: global)
    val.symbol = some result
proc GenParam*(_: typedesc[Symbol], id: Ident, val: Value, decl: GenTypeDef): Symbol =
    result = Symbol(kind: SymbolKind.GenParam, id: id, val: val, typ: val.typ, decl_gendef: decl, global: false)
    val.symbol = some result
proc Func*(_: typedesc[Symbol], id: Ident, typ: Value, global: bool): Symbol =
    result = Symbol(kind: SymbolKind.Func, id: id, typ: typ, global: global)
    typ.symbol = some result

proc empty*(_: typedesc[Symbol]): Symbol = Symbol()

import ../lineinfos
import identdefs
proc loc*(self: Symbol): Location =
    case self.kind
    of SymbolKind.Var, SymbolKind.Let, SymbolKind.Const, SymbolKind.Param:
        self.decl_iddef.loc
    of SymbolKind.Typ:
        self.decl_typedef.loc
    of SymbolKind.GenParam:
        self.decl_gendef.loc
    of SymbolKind.Func:
        self.decl_funcdef.id.loc
