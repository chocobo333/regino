
import strformat
import strutils


type
    VarOp {.pure.} = enum
        Decl
        Set
        Get
    TypeOp {.pure.} = enum
        Type
        Const
        Add
    Skind {.pure.} = enum
        Module = "module"
        Func = "func"
        Param = "param"
        Local = "local"
        Memory = "memory"
        I32 = "i32"
        F32 = "f32"
        F64 = "f64"
    SExpr = ref object
        case kind: Skind
        of Skind.Param, Skind.Local:
            typ: SExpr
        of Skind.Func:
            signature: seq[SExpr]
            locals: seq[SExpr]
            body: seq[SExpr]
        of I32, F32, F64:
            t_op*: TypeOp
            intval*: BiggestInt
            floatval*: BiggestFloat
        else:
            children: seq[SExpr]

proc Module*(_: typedesc[SExpr]): SExpr =
    SExpr(kind: Skind.Module)
proc Module*(_: typedesc[SExpr], ch: seq[SExpr]): SExpr =
    SExpr(kind: Skind.Module, ch: ch)
proc Func*(_: typedesc[SExpr], signature: seq[SExpr], locals: seq[SExpr], body: seq[SExpr]): SExpr =
    SExpr(kind: Skind.Func, signature: signature, locals: locals, body: body)
proc Param*(_: typedesc[SExpr], typ: SExpr): SExpr =
    SExpr(kind: Skind.Param, typ: typ)
proc Local*(_: typedesc[SExpr], typ: SExpr): SExpr =
    SExpr(kind: Skind.Local, typ: typ)
proc I32*(_: typedesc[SExpr]): SExpr =
    SExpr(kind: Skind.I32, t_op: TypeOp.Type)
proc F32*(_: typedesc[SExpr]): SExpr =
    SExpr(kind: Skind.F32, t_op: TypeOp.Type)
proc F64*(_: typedesc[SExpr]): SExpr =
    SExpr(kind: Skind.F64, t_op: TypeOp.Type)
proc Const(_: typedesc[SExpr], typ: range[Skind.I32..Skind.I32], intval: BiggestInt): SExpr =
    SExpr(kind: typ, t_op: TypeOp.Const, intval: intval)
proc Const(_: typedesc[SExpr], typ: range[Skind.F32..Skind.F32], floatval: BiggestFloat): SExpr =
    SExpr(kind: typ, t_op: TypeOp.Const, floatval: floatval)

proc add*(self: SExpr, ch: SExpr|seq[SExpr]) =
    self.children.add ch

proc `$`*(self: SExpr): string =
    case self.kind
    of Skind.Module:
        let ch = self.children.join("\n").indent(2)
        &"({self.kind}\n{ch}\n)"
    of Skind.Func:
        let sig = self.signature.join(" ")
        let loc = self.locals.join(" ")
        let body = self.body.join("\n").indent(2)
        &"({self.kind}\n  {sig}\n  {loc}\n{body}\n)"
    of Skind.Param, Skind.Local:
        fmt"({self.kind} {self.typ})"
    of Skind.I32:
        case self.t_op
        of TypeOp.Type:
            "i32"
        of TypeOp.Const:
            fmt"i32.const {self.intval}"
        of TypeOp.Add:
            "i32.add"
    of Skind.F32:
        case self.t_op
        of TypeOp.Type:
            "f32"
        of TypeOp.Const:
            fmt"f32.const {self.floatval}"
        of TypeOp.Add:
            "f32.add"
    of Skind.F64:
        case self.t_op
        of TypeOp.Type:
            "f64"
        of TypeOp.Const:
            fmt"f64.const {self.floatval}"
        of TypeOp.Add:
            "f64.add"
    else:
        ""


when isMainModule:
    let
        module = SExpr.Module
    module.add SExpr.Func(@[SExpr.Param(SExpr.I32)], @[SExpr.Local(SExpr.F32)], @[SExpr.Const(Skind.I32, 8)])


    echo module
