
import strformat
import strutils


type
    WType* {.pure.} = enum
        I32 = "i32"
        F32 = "f32"
        F64 = "f64"
    Skind {.pure.} = enum
        Module = "module"
        Func = "func"
        Param = "param"
        Local = "local"
        Memory = "memory"
    SExpr = ref object
        case kind: Skind
        of Skind.Param, Skind.Local:
            ty: WType
        of Skind.Func:
            signature: seq[SExpr]
            locals: seq[SExpr]
            body: seq[SExpr]
        else:
            children: seq[SExpr]

proc Module*(_: typedesc[SExpr]): SExpr =
    SExpr(kind: Skind.Module)
proc Module*(_: typedesc[SExpr], ch: seq[SExpr]): SExpr =
    SExpr(kind: Skind.Module, ch: ch)
proc Func*(_: typedesc[SExpr], signature: seq[SExpr], locals: seq[SExpr], body: seq[SExpr]): SExpr =
    SExpr(kind: Skind.Func, signature: signature, locals: locals, body: body)
proc Param*(_: typedesc[SExpr], ty: WType): SExpr =
    SExpr(kind: Skind.Param, ty: ty)
proc Local*(_: typedesc[SExpr], ty: WType): SExpr =
    SExpr(kind: Skind.Local, ty: ty)

proc add*(self: SExpr, ch: SExpr|seq[SExpr]) =
    self.children.add ch

proc `$`*(self: SExpr): string =
    case self.kind
    of Skind.Module:
        let ch = self.children.join(" ")
        fmt"({self.kind}{ch})"
    of Skind.Func:
        let sig = self.signature.join(" ")
        let loc = self.locals.join(" ")
        let body = self.body.join(" ")
        fmt"({self.kind}{sig}{loc}{body})"
    of Skind.Param, Skind.Local:
        fmt"({self.kind} {self.ty})"
    else:
        ""


when isMainModule:
    let
        module = SExpr.Module
    module.add SExpr.Func(@[SExpr.Param(WType.I32)], @[SExpr.Local(WType.I32)], @[])


    echo module
