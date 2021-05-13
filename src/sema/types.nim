
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
        