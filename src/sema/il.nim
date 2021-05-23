
## The module for internal language

import strformat
import strutils
import sequtils

import types
import symbols

import ../lineinfos


type
    TermKind* {.pure.} = enum
        Unit
        Bool
        Int
        String
        Id
        Let
        TypeDef
        FuncDef
        Lam
        App
        If
        Seq
        TypeOf
        Metadata
    Term* = ref object
        lineInfo*: LineInfo
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
            id*: Identifier
        of TermKind.Let..TermKind.TypeDef:
            name*: Identifier
            default*: Term
        of TermKind.FuncDef:
            fn*: Function
        of TermKind.Lam:
            param*: string
            body*: Term
        of TermKind.App:
            callee*: Term
            args*: seq[Term]
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

    Identifier* = ref object
        name*: string
        symbol*: Symbol
    Function* = ref object
        name*: string
        params*: seq[ParamDef]
        rety*: Term
        metadata*: Metadata
        body*: Term
    ParamDef* = ref object
        name*: string
        typ*: Term
    MetadataKind* {.pure.} = enum
        Link
        ImportLL
        UserDef
    Metadata* = ref object
        case kind*: MetadataKind
        of MetadataKind.Link..MetadataKind.ImportLL:
            nil
        of UserDef:
            name*: string
        param*: Term
    BuildInMetadata = Link..ImportLL
    FunctionMetadata = ImportLL..ImportLL
    NoBodyMetadata = ImportLL..ImportLL


proc Unit*(typ: typedesc[Term]): Term =
    Term(kind: TermKind.Unit)
proc Bool*(typ: typedesc[Term], b: bool): Term =
    Term(kind: TermKind.Bool, b: b)
proc Int*(typ: typedesc[Term], i: BiggestInt): Term =
    Term(kind: TermKind.Int, i: i)
proc String*(typ: typedesc[Term], s: string): Term =
    Term(kind: TermKind.String, s: s)
proc Id*(typ: typedesc[Term], name: string): Term =
    Term(kind: TermKind.Id, id: Identifier(name: name))
proc Let*(typ: typedesc[Term], name: string, default: Term): Term =
    Term(kind: TermKind.Let, name: Identifier(name: name), default: default)
proc TypeDef*(typ: typedesc[Term], name: string, default: Term): Term =
    Term(kind: TermKind.TypeDef, name: Identifier(name: name), default: default)
proc FuncDef*(typ: typedesc[Term], fn: Function): Term =
    Term(kind: TermKind.FuncDef, fn: fn)
proc Lam*(typ: typedesc[Term], param: string, body: Term): Term =
    Term(kind: TermKind.Lam, param: param, body: body)
proc App*(typ: typedesc[Term], callee: Term, args: seq[Term]): Term =
    Term(kind: TermKind.App, callee: callee, args: args)
proc If*(typ: typedesc[Term], cond: Term, thent: Term, elset: Term): Term =
    Term(kind: TermKind.If, cond: cond, thent: thent, elset: elset)
proc Seq*(typ: typedesc[Term], ts: seq[Term]): Term =
    Term(kind: TermKind.Seq, ts: ts)
proc TypeOf*(typ: typedesc[Term], t: Term): Term =
    Term(kind: TermKind.TypeOf, typeof: t)
proc Metadat*(typ: typedesc[Term], metadata: Metadata): Term =
    Term(kind: TermKind.Metadata, metadata: metadata)

proc Link*(typ: typedesc[Metadata], param: Term): Metadata =
    Metadata(kind: MetadataKind.Link, param: param)
proc ImportLL*(typ: typedesc[Metadata], param: Term): Metadata =
    Metadata(kind: MetadataKind.ImportLL, param: param)
proc UserDef*(typ: typedesc[Metadata], name: string, param: Term): Metadata =
    Metadata(kind: MetadataKind.UserDef, name: name, param: param)

proc newFunction*(name: string, params: seq[ParamDef], rety: Term, body: Term, metadata: Metadata = nil): Function =
    Function(name: name, params: params, rety: rety, body: body, metadata: metadata)


proc `$`*(self: Term): string
proc `$`*(self: ParamDef): string =
    fmt"{self.name}: {self.typ}"
proc `$`*(self: Metadata): string =
    let 
        tmp = if self.param.isNil: "" else: fmt": {self.param}"
        name = case self.kind
        of MetadataKind.Link:
            "link"
        of MetadataKind.ImportLL:
            "importll"
        of MetadataKind.UserDef:
            self.name
    fmt"![{name}{tmp}]"
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
        self.id.name
    of TermKind.Let:
        fmt"let {self.name.name} = {self.default}"
    of TermKind.TypeDef:
        fmt"type {self.name.name} = {self.default}"
    of TermKind.FuncDef:
        let
            rety = if self.fn.rety.isNil: "" else: fmt" -> {self.fn.rety}"
            param = self.fn.params.map(`$`).join(", ")
            metadata = if self.fn.metadata.isNil: "" else: fmt" {self.fn.metadata}"
        fmt"func {self.fn.name}({param}){rety}{metadata}:" & ("\n" & $self.fn.body).indent(2)
    of TermKind.Lam:
        fmt"λ{self.param}.{self.body}" 
    of TermKind.App:
        let
            args = self.args.join(", ")
        fmt"{self.callee}({args})"
    of TermKind.If:
        fmt"if {self.cond} then {self.thent} else {self.elset}"
    of TermKind.Seq:
        self.ts.join("\n")
    of TermKind.TypeOf:
        fmt"typeof({self.typeof})"
    of TermKind.Metadata:
        $self.metadata
