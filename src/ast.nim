
import strformat
import strutils
import sequtils
import tables

import coloredString

import eat/spanned
import eat/utils

import lineInfos


type
    AstKind* = enum
        akFailed
        akEmpty
        akComment
        akStmtList
        akLetSection
        akVarSection
        akConstSection
        akAliasSection
        # akDefinition
        akFuncDef
        akTempDef
        akMacroDef
        akIterDef
        akIdentDef
        akAsign
        akBlockExpr
        akIfExpr
        akWhenExpr
        akElifBranch
        akElseBranch
        akLoopExpr
        akInfix
        akPrefix
        akPostfix
        akDot
        akCommand
        akDiscardPattern
        akIdPattern
        akLiteralPattern
        akPatterns
        akCall
        akChar
        akInt
        akFloat
        akString
        akId
    # DefKind = enum
    #     Func
    #     Template
    #     Macro
    #     Iterator

    TypeKind* = enum
        tkNone
        tkUnit
        tkInt
        tkFloat
        tkChar
        tkString
        tkStruct
        tkRecord
        tkTuple
        tkVariant
        tkEnum
        tkType
        tkFunc
        tkApp
        tkVar
        tkGen

    SymbolKind* = enum
        skVar
        skType
        skFunc

const
    akLiteral* = {akChar..akString}
    akHasChildren* = {akStmtList..akCall}
    akStatement* = {akLetSection..akAsign}
    akExpr* = {akBlockExpr..akCommand, akCall..akId}
    akPat* = {akDiscardPattern..akPatterns}

type
    AstNode* = ref object
        lineInfo*: LineInfo
        sym*: Symbol
        typ*: Type
        case kind*: AstKind
        of akChar..akInt:
            intVal*: BiggestInt
        of akFloat:
            floatVal*: BiggestFloat
        of akString..akId:
            strVal*: string
        else:
            children*: seq[AstNode]

    TypeVar* = object
        id*: TypeVarId
    TypeSubstitution* = seq[(TypeVar, Type)]

    TypeSchemeKind* = enum
        tskForall
    TypeScheme* = object
        case kind: TypeSchemeKind
        of tskForall:
            nGen: int
            typ: Type
    TypeAssumption* = object
        id: TypeAssumptionId
        scheme: TypeScheme

    Type* = ref object
        name*: string   # TODO: Is this necessary?
        sym: Symbol     # contains name information
        size: BiggestInt    # the size of the type in bits
                            # -1 means that the size is unkwown
        case kind: TypeKind
        of tkStruct, tkRecord, tkTuple:
            fields: seq[(string, Type)]
        of tkVariant:
            variants: seq[(string, seq[Type])]
        of tkType:
            typ*: Type
        of tkFunc:
            paramtype*: seq[Type]
            rettype*: Type
        of tkApp:
            base: Type
            params: seq[Type]
        of tkVar:
            tvar: TypeVar
        of tkGen:
            id*: TypeGenId
        else:
            nil

    Symbol* = ref object
        lineInfo*: LineInfo
        name*: string
        id*: int
        kind*: SymbolKind
        typ*: Type

    SymId* = int
    TypeVarId* = int
    TypeGenId* = int
    TypeAssumptionId* = int
    Environment* = ref object
        symid: SymId
        typevarid: TypeVarId
        syms*: Table[string, seq[Symbol]]

    Types* = Type | TypeScheme | TypeAssumption | seq[Types]

proc add*(self: var TypeSubstitution, tvar: TypeVar, typ: Type): TypeSubstitution =
    self.add (tvar, typ)
    self

# TODO:
proc apply*[T: Types](sbst: TypeSubstitution, a: T): T =
    when T is seq:
        a.mapIt(apply(sbst, it))
    elif T is Type:
        proc tvar*(sbst: TypeSubstitution, tyvar: TypeVar): Type =
            for tvar, ty in sbst:
                if tvar == tyvar:
                    return ty
            return tyvar
        case self.kind
        of tkNone..tkString:
            discard
        of tkStruct, tkRecord, tkTuple:
            result = apply(self.fields.mapIt(it[1]))
        of tkVariant:
            discard
        of tkEnum:
            discard
        of tkType:
            discard
        of tkFunc:
            discard
        of tkApp:
            result = Type(kind: tkApp, base: apply(sbst, a.base), param: apply(sbst, self.param))
        of tkVar:
            result = tvar(sbst, a)
        of tkGen:
            discard
    return a
# TODO: 
proc tv*[T: Types](self: T): seq[TypeVar] =
    when T is seq:
        result = @[]
        for e in self:
            result.add tv e
    elif T is Type:
        case self.kind
        of tkNone..tkString:
            discard
        of tkStruct, tkRecord, tkTuple:
            result = tv(self.fields.mapIt(it[1]))
        of tkVariant:
            discard
        of tkEnum:
            discard
        of tkType:
            discard
        of tkFunc:
            discard
        of tkApp:
            result = tv self.base & tv self.param
        of tkVar:
            result = @[self.tvar]
        of tkGen:
            discard
    return @[]


proc newEnvironment*(): Environment =
    Environment()

proc newSymId*(self: Environment): SymId =
    inc self.symid
    self.symid

proc newTypeVarId*(self: Environment): TypeVarId =
    inc self.typevarid
    self.typevarid

proc addSym*(self: Environment, name: string, sym: Symbol) =
    if name in self.syms:
        self.syms[name].add sym
    else:
        self.syms[name] = @[sym]

proc `$`*(self: AstNode): string =
    let k = fmt"{($self.kind)[2..^1].green} {""@"".cyan} {($self.lineInfo)}"
    case self.kind:
    of akChar..akInt:
        genGraph(k, self.intVal)
    of akFloat:
        genGraph(k, self.floatVal)
    of akString..akId:
        genGraph(k, &"\"{self.strVal}\"")
    else:
        genGraphS(k, self.children)

proc `$`*(self: Type): string =
    case self.kind
    of tkNone:
        "`None`"
    of tkUnit:
        "()"
    of tkInt:
        "i" & $self.size
    of tkFloat:
        "f" & $self.size
    of tkChar:
        "char"
    of tkString:
        "string"
    of tkStruct:
        ""
    of tkRecord:
        ""
    of tkTuple:
        ""
    of tkVariant:
        ""
    of tkEnum:
        ""
    of tkType:
        ""
    of tkFunc:
        ""
    of tkApp:
        ""
    of tkVar:
        fmt"TypeVar{self.tvar}"
    of tkGen:
        ""

proc `$`*(self: Symbol): string =
    result = self.name
    if not self.typ.isNil:
        result &= fmt"(: {self.typ})"
    let tmp = case self.kind
    of skVar:
        "Variable"
    of skFunc:
        "Function"
    of skType:
        "Type"
    result &= fmt"({tmp}, id: {self.id})"
    result &= fmt" @ {self.lineInfo}"


proc newIntNode*(val: BiggestInt, info: LineInfo = newLineInfo(-1, newPosition(), newPosition())): AstNode =
    AstNode(kind: akInt, intVal: val, lineInfo: info)

proc newFloatNode*(val: BiggestFloat, info: LineInfo = newLineInfo(-1, newPosition(), newPosition())): AstNode =
    AstNode(kind: akFloat, floatVal: val, lineInfo: info)

proc newStrNode*(val: string, info: LineInfo = newLineInfo(-1, newPosition(), newPosition())): AstNode =
    AstNode(kind: akString, strVal: val, lineInfo: info)

proc newIdNode*(name: string, info: LineInfo = newLineInfo(-1, newPosition(), newPosition())): AstNode =
    AstNode(kind: akId, strVal: name, lineInfo: info)

proc newTreeNode*(kind: range[akFailed..akCall], children: seq[AstNode], info: LineInfo = newLineInfo(children[0].lineInfo.fileid, children[0].lineInfo.span.a, children[^1].lineInfo.span.b)): AstNode =
    AstNode(kind: kind, children: children, lineInfo: info)

proc newNode*(kind: AstKind, info: LineInfo = newLineInfo(-1, newPosition(), newPosition())): AstNode =
    AstNode(kind: kind, lineInfo: info)

proc newEmptyNode*(): AstNode = akEmpty.newNode()
proc newFailedNode*(): AstNode = akFailed.newNode()
proc empty*(t: typedesc[AstNode]): AstNode = newEmptyNode()

proc isEmpty*(self: AstNode): bool =
    self.kind == akEmpty

proc seta*(self: AstNode, a: Position): AstNode =
    result = self
    result.lineInfo.span.a = a

proc setb*(self: AstNode, b: Position): AstNode =
    result = self
    result.lineInfo.span.b = b

proc repr*(self: AstNode, ind: uint = 2): string =
    proc repr2(self: AstNode): string = repr(self, ind)
    result = case self.kind
    of akFailed:
        ""
    of akEmpty:
        ""
    of akComment:
        ""
    of akStmtList:
        self.children.map(repr2).join("\n")
    of akLetSection:
        "let\n" & self.children.map(repr2).join("\n").indent(ind)
    of akVarSection:
        "var\n" & self.children.map(repr2).join("\n").indent(ind)
    of akConstSection:
        "const\n" & self.children.map(repr2).join("\n").indent(ind)
    of akAliasSection:
        "alias\n" & self.children.map(repr2).join("\n").indent(ind)
    # of akDefinition:
    #     ""
    of akFuncDef:
        ""
    of akTempDef:
        ""
    of akMacroDef:
        ""
    of akIterDef:
        ""
    of akIdentDef:
        let
            typ = if self.children[1].isEmpty: "" else: &": {self.children[1].repr}"
            default = if self.children[2].isEmpty: "" else: &" = {self.children[2].repr}"
        &"{self.children[0].repr}{typ}{default}"
    of akAsign:
        ""
    of akBlockExpr:
        ""
    of akIfExpr:
        self.children.map(repr2).join("\n")[2..^1]
    of akWhenExpr:
        ""
    of akElifBranch:
        let branch = self.children[1].repr2.indent(2)
        &"elif {self.children[0].repr2}:\n{branch}"
    of akElseBranch:
        let branch = self.children[0].repr2.indent(2)
        &"else:\n{branch}"
    of akLoopExpr:
        ""
    of akInfix:
        ""
    of akPrefix:
        ""
    of akPostfix:
        ""
    of akDot:
        ""
    of akCommand:
        var
            args = self.children[1..^1].map(repr2).join(", ")
        &"{self.children[0].repr} {args}"
    of akDiscardPattern:
        "_"
    of akIdPattern:
        self.children[0].repr2
    of akLiteralPattern:
        self.children[0].repr2
    of akPatterns:
        self.children.map(repr2).join(", ")
    of akCall:
        ""
    of akChar:
        (chr self.intVal).repr
    of akInt:
        $self.intVal
    of akFloat:
        $self.floatVal
    of akString:
        # "\"" & self.strVal & "\""
        self.strVal.repr
    of akId:
        self.strVal

    # TODO: remove it
    if not self.typ.isNil:
        result &= fmt"(: {self.typ})"

let
    charType = Type(kind: tkChar)
    stringType = Type(kind: tkString)
proc newCharType*(): Type =
    charType

proc newStringType*(): Type =
    stringType

proc newIntType*(size: BiggestInt = 64): Type =
    Type(kind: tkInt, size: size)
proc newFloatType*(size: BiggestInt = 64): Type =
    Type(kind: tkFloat, size: size)

proc newTypeVar*(env: Environment): TypeVar =
    TypeVar(id: env.newTypeVarId)
converter toType*(self: TypeVar): Type =
    Type(kind: tkVar, tvar: self)

proc newSymbol*(kind: SymbolKind, node: AstNode, env: Environment): Symbol =
    let
        id = env.newSymId
        name = node.strVal
    result = Symbol(
        lineInfo: node.lineInfo,
        name: node.strVal,
        id: id,
        kind: kind,
        typ: newTypeVar(env)
    )
    env.addSym(name, result)