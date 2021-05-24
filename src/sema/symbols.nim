
import strformat
import strutils

from types import PolyType, `$`

from llvm import Value


type
    SymbolKind* {.pure.} = enum
        Var
        Let
        Const
        Func
        Type
        Choice
    Symbol* = ref object
        typ*: PolyType
        case kind*: SymbolKind
        of Var..Type:
            val*: Value
            lty*: llvm.Type
        of Choice:
            syms*: seq[Symbol]

proc Var*(ty: typedesc[Symbol], typ: PolyType): Symbol =
    Symbol(kind: SymbolKind.Var, typ: typ)
proc Let*(ty: typedesc[Symbol], typ: PolyType): Symbol =
    Symbol(kind: SymbolKind.Let, typ: typ)
proc Const*(ty: typedesc[Symbol], typ: PolyType): Symbol =
    Symbol(kind: SymbolKind.Const, typ: typ)
proc Func*(ty: typedesc[Symbol], typ: PolyType): Symbol =
    Symbol(kind: SymbolKind.Func, typ: typ)
proc Typ*(ty: typedesc[Symbol], typ: PolyType): Symbol =
    Symbol(kind: SymbolKind.Type, typ: typ)
proc Choice*(ty: typedesc[Symbol], typ: PolyType, syms: seq[Symbol]): Symbol =
    let syms = block:
        var a: seq[Symbol] = @[]
        for e in syms:
            if e.kind == SymbolKind.Choice:
                a.add e.syms
            else:
                a.add e
        a
    Symbol(kind: SymbolKind.Choice, typ: typ, syms: syms)

proc `$`*(self: Symbol): string =
    case self.kind
    of SymbolKind.Var..SymbolKind.Type:
        fmt"{self.kind}{self.typ}"
    of SymbolKind.Choice:
        let tmp = self.syms.join(", ")
        fmt"Chioce({tmp})"