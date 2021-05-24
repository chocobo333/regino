
import sets
import hashes
import strformat
import strutils
import sequtils


type
    TypeKind* {.pure.} = enum
        Unit
        Bool
        Int
        String
        Arr
        Var
        TypeDesc
    Type* = ref object
        case kind*: TypeKind
        of TypeKind.Unit:
            nil
        of TypeKind.Bool:
            nil
        of TypeKind.Int:
            nil
        of TypeKind.String:
            nil
        of TypeKind.Arr:
            paramty*: seq[Type]
            rety*: Type
        of TypeKind.Var:
            v*: TypeVar
        of TypeKind.TypeDesc:
            typ*: Type

    PolyTypeKind* {.pure.} = enum
        ForAll
    PolyType* = ref object
        case kind*: PolyTypeKind
        of ForAll:
            gen*: HashSet[TypeVar]
            typ*: Type

    TypeVar* = ref object
        id*: int

proc Unit*(typ: typedesc[Type]): Type =
    Type(kind: TypeKind.Unit)
proc Bool*(typ: typedesc[Type]): Type =
    Type(kind: TypeKind.Bool)
proc Int*(typ: typedesc[Type]): Type =
    Type(kind: TypeKind.Int)
proc String*(typ: typedesc[Type]): Type =
    Type(kind: TypeKind.String)
proc Arr*(typ: typedesc[Type], paramty: seq[Type], rety: Type): Type =
    Type(kind: TypeKind.Arr, paramty: paramty, rety: rety)
proc Var*(typ: typedesc[Type], v: TypeVar): Type =
    Type(kind: TypeKind.Var, v: v)
proc TypeDesc*(typ: typedesc[Type], t: Type): Type =
    Type(kind: TypeKind.TypeDesc, typ: t)

proc ForAll*(ty: typedesc[PolyType], gen: HashSet[TypeVar], typ: Type): PolyType =
    PolyType(kind: PolyTypeKind.ForAll, gen: gen, typ: typ)

proc hash*(self: TypeVar): Hash =
    Hash self.id
proc hash*(self: Type): Hash =
    result = 0
    case self.kind
    of TypeKind.Unit:
        result = result !& ord(TypeKind.Unit)
    of TypeKind.Bool:
        result = result !& ord(TypeKind.Bool)
    of TypeKind.Int:
        result = result !& ord(TypeKind.Int)
    of TypeKind.String:
        result = result !& ord(TypeKind.String)
    of TypeKind.Arr:
        result = result !& ord(TypeKind.Arr)
        for e in self.paramty:
            result = result !& hash e
        result = result !& hash self.rety
    of TypeKind.Var:
        result = result !& ord(TypeKind.Var)
        result = result !& hash self.v
    of TypeKind.TypeDesc:
        result = result !& ord(TypeKind.TypeDesc)
        result = result !& hash self.typ
proc `$`*(self: TypeVar): string =
    let m = len('a'..'z')
    var p = self.id
    while p > 26:
        let q = p mod m
        p = p div m
        result.add chr(ord('a') + q)
    result.add chr(ord('a') + p)
proc `$`*(self: Type): string =
    case self.kind
    of TypeKind.Unit:
        "unit"
    of TypeKind.Bool:
        "bool"
    of TypeKind.Int:
        "int"
    of TypeKind.String:
        "string"
    of TypeKind.Arr:
        let
            params = self.paramty.join(", ")
            params2 = if self.paramty.len == 1: params else: fmt"({params})"
        fmt"{params2} -> {self.rety}"
    of TypeKind.Var:
        $self.v
    of TypeKind.TypeDesc:
        fmt"typedesc[{self.typ}]"

proc `$`*(self: PolyType): string =
    case self.kind
    of PolyTypeKind.ForAll:
        let tmp = if self.gen.len == 0: "" else: "∀" & toSeq(self.gen.items).join(".∀") & "."
        fmt"{tmp}{self.typ}"