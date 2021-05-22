
## The module for internal language

import strformat
import strutils

import types


type
    TermKind* {.pure.} = enum
        Unit
        Bool
        Int
        String
        Id
        Let
        FuncDef
        Lam
        App
        If
        Seq
        TypeOf
        Metadata
    Term* = ref object
        typ*: Type
        case kind*: TermKind
        of TermKind.Unit:
            nil
        of TermKind.Bool:
            b*: bool
        of TermKind.Int:
            i*: BiggestInt
        of TermKind.String:
            s*: string
        of TermKind.Id:
            name*: string
        of TermKind.Let:
            id*: string
            default*: Term
        of TermKind.FuncDef:
            fname*: string
            paramname*: string
            paramty*: Term
            rety*: Term
            fbody*: Term
        of TermKind.Lam:
            param*: string
            body*: Term
        of TermKind.App:
            callee*: Term
            arg*: Term
        of TermKind.If:
            cond*: Term
            thent*: Term
            elset*: Term
        of TermKind.Seq:
            ts*: seq[Term]
        of TermKind.TypeOf:
            typeof*: Term
        of TermKind.Metadata:
            metadata*: Metadata

    Metadata* = ref object
        name*: string
        param*: Term


proc Unit*(typ: typedesc[Term]): Term =
    Term(kind: TermKind.Unit)
proc Bool*(typ: typedesc[Term], b: bool): Term =
    Term(kind: TermKind.Bool, b: b)
proc Int*(typ: typedesc[Term], i: BiggestInt): Term =
    Term(kind: TermKind.Int, i: i)
proc String*(typ: typedesc[Term], s: string): Term =
    Term(kind: TermKind.String, s: s)
proc Id*(typ: typedesc[Term], name: string): Term =
    Term(kind: TermKind.Id, name: name)
proc Let*(typ: typedesc[Term], id: string, default: Term): Term =
    Term(kind: TermKind.Let, id: id, default: default)
proc FuncDef*(typ: typedesc[Term], fname: string, paramname: string, paramty: Term, rety: Term, fbody: Term): Term =
    Term(kind: TermKind.FuncDef, fname: fname, paramname: paramname, paramty: paramty, rety: rety, fbody: fbody)
proc Lam*(typ: typedesc[Term], param: string, body: Term): Term =
    Term(kind: TermKind.Lam, param: param, body: body)
proc App*(typ: typedesc[Term], callee: Term, arg: Term): Term =
    Term(kind: TermKind.App, callee: callee, arg: arg)
proc If*(typ: typedesc[Term], cond: Term, thent: Term, elset: Term): Term =
    Term(kind: TermKind.If, cond: cond, thent: thent, elset: elset)
proc Seq*(typ: typedesc[Term], ts: seq[Term]): Term =
    Term(kind: TermKind.Seq, ts: ts)
proc TypeOf*(typ: typedesc[Term], t: Term): Term =
    Term(kind: TermKind.TypeOf, typeof: t)
proc metadata*(typ: typedesc[Term], metadata: Metadata): Term =
    Term(kind: TermKind.Metadata, metadata: metadata)

proc `$`*(self: Term): string
proc `$`*(self: Metadata): string =
    let tmp = if self.param.isNil: "" else: fmt": {self.param}"
    fmt"![{self.name}{tmp}]"
proc `$`*(self: Term): string =
    case self.kind
    of TermKind.Unit:
        "()"
    of TermKind.Bool:
        $self.b
    of TermKind.Int:
        $self.i
    of TermKind.String:
        "\"" & self.s & "\""
    of TermKind.Id:
        self.name
    of TermKind.Let:
        fmt"let {self.id} = {self.default}"
    of TermKind.FuncDef:
        let rety = if self.rety.isNil: "" else: fmt" -> {self.rety}"
        fmt"func {self.fname}({self.paramname}: {self.paramty}){rety}:" & ("\n" & $self.fbody).indent(2)
    of TermKind.Lam:
        fmt"λ{self.param}.{self.body}" 
    of TermKind.App:
        fmt"{self.callee}({self.arg})"
    of TermKind.If:
        fmt"if {self.cond} then {self.thent} else {self.elset}"
    of TermKind.Seq:
        self.ts.join("\n")
    of TermKind.TypeOf:
        fmt"typeof({self.typeof})"
    of TermKind.Metadata:
        $self.metadata
