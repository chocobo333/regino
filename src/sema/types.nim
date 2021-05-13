
import tables
import hashes
import sequtils

import ../lineInfos


type
    TypeKind* = enum
        tkNone
        tkUnit
        tkChar
        tkInt
        tkFloat
        tkString
        tkBool
        tkApp
        tkFunc
        tkGen
        tkVar
    Type* = ref object
        sym*: Symbol
        case kind*: TypeKind
        of tkNone, tkUnit:
            nil
        of tkChar..tkBool:
            nil
        of tkApp:
            base*: Type
            types*: seq[Type]
        of tkFunc:
            rety*: Type
            paramty*: seq[Type]
        of tkGen:
            tgid*: TypeGenId
        of tkVar:
            v*: TypeVar
    TypeVar* = ref object
        id*: TypeVarId
    TypeSchemeKind* = enum
        tskAll
    TypeScheme* = ref object
        ngen*: int
        typ*: Type
        kind*: TypeSchemeKind
        tsid*: TypeSchemeId
    SymbolKind* = enum
        skChoice
        skFunc
        skType
        skVar
        skLet
        skConst
    Symbol* = ref object
        case kind*: SymbolKind
        of skChoice:
            syms*: seq[Symbol]
        else:
            id*: Id
            typ*: Type
    Id* = ref object
        lineInfo*: LineInfo
        name*: string
        sym*: Symbol
        # TODO: other infos
    TypeGenId* = int
    TypeVarId* = int
    TypeSchemeId* = int

    SymEnv* = ref object
        parent*: SymEnv
        env*: Table[string, Symbol]
    TypeEnv* = ref object
        typevarid*: TypeVarId
        typeschemeid*: TypeSchemeId
        symenv*: SymEnv
        # typesubstituions
        tvenv*: Table[TypeVar, Type] # seq[(TypeVar, Type)], Table[TypeVar, seq[Type]]
    TypeSubstitution = Table[TypeVar, Type]
    TypeAssumption = ref object
        id: Id
        types: seq[TypeScheme]
    Types = concept T, var t
        apply(TypeSubstitution, T)
        t.typevars() is seq[TypeVar]

# TypeVar
proc hash*(self: TypeVar): Hash = hash self.id

# SymEnv
proc newSymEnv*(): SymEnv =
    SymEnv(env: initTable[string, Symbol]())

# TypeEnv
proc newTypeEnv*(): TypeEnv =
    TypeEnv(
        typevarid: 0,
        typeschemeid: 0,
        symenv: newSymEnv(),
        tvenv: initTable[TypeVar, Type]()
    )

proc newTypeVarId(self: TypeEnv): TypeVarId =
    result = self.typevarid
    inc self.typevarid
proc newTypeVar(self: TypeEnv): TypeVar =
    let id = self.newTypeVarId()
    TypeVar(id: id)

proc newTypeSchemeId(self: TypeEnv): TypeSchemeId =
    result = self.typeschemeid
    inc self.typeschemeid

# TypeSubstition
let nullSubst = initTable[TypeVar, Type]()
proc lookup(self: TypeSubstitution, t: TypeVar): Type =
    if t in self:
        self[t]
    else:
        Type(kind: tkVar, v: t)

proc apply(self: TypeSubstitution, t: Type): Type =
    case t.kind
    of tkNone..tkBool:
        t
    of tkApp:
        Type(kind: tkApp, base: self.apply(t.base), types: t.types.mapIt(self.apply(it)))
    of tkFunc:
        Type(kind: tkFunc, rety: self.apply(t.rety), paramty: t.paramty.mapIt(self.apply(it)))
    of tkGen:
        t
    of tkVar:
        let u = self.lookup(t.v)
        if t == u:
            return t
        else:
            self.apply(u)
proc apply*(self: TypeSubstitution, t: TypeScheme): TypeScheme =
    result = t
    t.typ = self.apply(t.typ)
proc apply*(self: TypeSubstitution, t: TypeAssumption): TypeAssumption =
    result = t
    t.types = t.types.mapIt(self.apply(it))
proc apply*[T: Types](self: TypeSubstitution, t: seq[T]): seq[T] =
    t.mapIt(self.apply(it))
proc extend(self: var TypeSubstitution, tv: TypeVar, t: Type) =
    if tv in self:
        assert false
    else:
        self[tv] = t

proc typevars*[T: Types](self: seq[T]): seq[TypeVar]
proc typevars*(self: Type): seq[TypeVar] =
    case self.kind
    of tkNone..tkBool:
        @[]
    of tkApp:
        (self.base.typevars() & self.types.map(typevars)).concat.deduplicate()
    of tkFunc:
        (self.rety.typevars() & self.paramty.map(typevars)).concat.deduplicate()
    of tkGen:
        @[]
    of tkVar:
        @[self.v]
proc typevars*(self: TypeScheme): seq[TypeVar] =
    case self.kind
    of tskAll:
        self.typ.typevars()
proc typevars*(self: TypeAssumption): seq[TypeVar] {.error.} =
    self.types.typevars()
proc typevars*[T: Types](self: seq[T]): seq[TypeVar] =
    self.map(typevars).concat.deduplicate()

proc gen*(asumps: seq[TypeAssumption], t: Type): TypeScheme =
    let gs = typevars


assert Type is Types
assert TypeScheme is Types
assert TypeAssumption is Types
assert seq[Type] is Types
assert seq[TypeScheme] is Types
assert seq[TypeAssumption] is Types