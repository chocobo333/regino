
import sets
import hashes
import strformat


type
    TypeKind* {.pure.} = enum
        Unit
        Bool
        Int
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
        of TypeKind.Arr:
            paramty*: Type
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
proc Arr*(typ: typedesc[Type], paramty, rety: Type): Type =
    Type(kind: TypeKind.Arr, paramty: paramty, rety: rety)
proc Var*(typ: typedesc[Type], v: TypeVar): Type =
    Type(kind: TypeKind.Var, v: v)
proc TypeDesc*(typ: typedesc[Type], t: Type): Type =
    Type(kind: TypeKind.TypeDesc, typ: t)

proc ForAll*(ty: typedesc[PolyType], gen: HashSet[TypeVar], typ: Type): PolyType =
    PolyType(kind: PolyTypeKind.ForAll, gen: gen, typ: typ)

proc hash*(self: TypeVar): Hash =
    Hash self.id
proc `$`*(self: TypeVar): string =
    fmt"{self.id}'"
proc `$`*(self: Type): string =
    case self.kind
    of TypeKind.Unit:
        "unit"
    of TypeKind.Bool:
        "bool"
    of TypeKind.Int:
        "int"
    of TypeKind.Arr:
        fmt"{self.paramty} -> {self.rety}"
    of TypeKind.Var:
        $self.v
    of TypeKind.TypeDesc:
        fmt"typedesc[{self.typ}]"

proc `$`*(self: PolyType): string =
    case self.kind
    of PolyTypeKind.ForAll:
        $self.typ