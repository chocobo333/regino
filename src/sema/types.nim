
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
    # TypeEnv* = ref object
    #     typevarid*: TypeVarId
    #     typeschemeid*: TypeSchemeId
    #     symenv*: SymEnv
    #     # typesubstituions
    #     tvenv*: Table[TypeVar, Type] # seq[(TypeVar, Type)], Table[TypeVar, seq[Type]]
    TypeSubstitution = Table[TypeVar, Type]
    TypeAssumption = ref object
        id: Id
        types: seq[TypeScheme]
    Types = concept T, var t
        apply(TypeSubstitution, T)
        t.typevars() is seq[TypeVar]

    SymTable = Table[string, Type]

    TypeEnv* = ref object
        parent*: TypeEnv
        table*: SymTable

# SymEnv
proc newSymEnv*(): SymEnv =
    SymEnv(env: initTable[string, Symbol]())

# TypeEnv
proc newTypeEnv*(): TypeEnv =
    TypeEnv(
        parent: nil,
        table: initTable[string, Type]()
    )
        
proc extend*(typeEenv: TypeEnv, name: string, typ: Type) = 
    typeEenv.table[name] = typ

proc lookup*(typeEnv: TypeEnv, name: string): Type =
    if name in typeEnv.table:
        return typeEnv.table[name]
    elif typeEnv.parent.isNil:
        # TODO: raise error
        return Type(kind: tkNone)
    else:
        return lookup(typeEnv.parent, name)

proc addScope*(typeEnv: TypeEnv): TypeEnv = 
    var ret = newTypeEnv()
    ret.parent = typeEnv
    ret