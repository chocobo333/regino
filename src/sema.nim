
import tables
import hashes
import sequtils
import strutils
import strformat

import ast
import lineInfos


type
    Program* = ref object
        stmts*: StmtList

    StmtList* = seq[Statement]
    StatementKind* = enum
        stkComment
        stkVarDecl
        stkLetDecl
        stkConstDecl
        stkFuncDef
        stkTempDef
        stkMacroDef
        stkIterDef
        stkExprStmt
        stkAsign
    Statement* = ref object
        lineInfo: LineInfo
        case kind*: StatementKind
        of stkComment:
            comment: string
        of stkVarDecl, stkLetDecl, stkConstDecl:
            iddefs*: seq[IdentDef]
        of stkFuncDef, stkTempDef, stkMacroDef, stkIterDef:
            name*: string
            rety*: Type
            paramty*: seq[Type]
            params*: seq[IdentDef]
            body*: Expr
        of stkExprStmt:
            exp*: Expr
        of stkAsign:
            lhs*: Pattern
            rhs*: Expr
    IdentDef* = ref object
        id*: Pattern
        typ*: Expr
        default*: Expr
    PatternKind* = enum
        pkDiscard
        pkId
        pkLiteral
        pkPatterns
    Pattern* = ref object
        case kind*: PatternKind
        of pkDiscard:
            nil
        of pkId:
            id*: Id
        of pkLiteral:
            lit*: Expr
        of pkPatterns:
            pats*: seq[Pattern]
    Id* = ref object
        lineInfo: LineInfo
        name*: string
        sym*: Symbol
        # TODO: other infos
    ExprKind* = enum
        ekCall
        ekBinOp
        ekIfExpr
        ekLoopExpr
        ekBlockExpr
        ekChar
        ekInt
        ekFloat
        ekString
        ekSym
    Expr* = ref object
        lineInfo: LineInfo
        case kind*: ExprKind
        of ekCall:
            callee*: Expr
            params*: seq[Expr]
        of ekBinOp:
            op*: Expr
            lhs*: Expr
            rhs*: Expr
        of ekIfExpr:
            elifbranches*: seq[ElifBranch]
            elsebranch*: StmtList
        of ekLoopExpr, ekBlockExpr:
            body*: StmtList
        of ekChar:
            charval*: char
        of ekInt:
            intval*: BiggestInt
            intsize*: int
        of ekFloat:
            floatval*: BiggestFloat
            floatsize*: int
        of ekString:
            strval*: string
        of ekSym:
            name*: string
            sym*: Symbol
    ElifBranch* = ref object
        cond*: Expr
        branch*: StmtList
    TypeKind* = enum
        tkNone
        # tkUnit
        tkCon
        tkApp
        tkFunc
        tkGen
        tkVar
        tkAll
    Type* = ref object
        sym*: Symbol
        case kind*: TypeKind
        of tkNone:
            nil
        of tkCon:
            name*: string
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
        of tkAll:
            typ*: Type
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
            typ*: Type
    TypeVarId* = int
    TypeSchemeId* = int

    SymEnv = ref object
        parent*: SymEnv
        env*: Table[string, Symbol]
    TypeEnv* = ref object
        typevarid: TypeVarId
        typeschemeid: TypeSchemeId
        symenv: SymEnv
        # typesubstituions
        tvenv: Table[TypeVar, Type] # seq[(TypeVar, Type)], Table[TypeVar, seq[Type]]

# Type
proc `$`*(self: Type): string =
    case self.kind
    of tkNone:
        "none"
    of tkCon:
        self.name
    of tkApp:
        let tmp = self.types.join(", ")
        fmt"{self.base}[{tmp}]"
    of tkFunc:
        let tmp = self.paramty.join(", ")
        fmt"({tmp}) -> {self.rety}"
    of tkGen:
        fmt"TypeGen(id: {self.tgid}"
    of tkVar:
        fmt"TypeVar(id: {self.v.id})"
    of tkAll:
        fmt"ForAll"
proc hash*(self: Type): Hash =
    result = 0
    result = !$ result
proc newNoneType*(): Type =
    Type(kind: tkNone)
# Symbol
proc hash*(self: Symbol): Hash =
    result = 0
    result = !$result
proc newSymbol(kind: range[skFunc..skConst]): Symbol =
    Symbol(kind: kind)
proc newChoice(syms: seq[Symbol]): Symbol =
    let syms = block:
        var tmp: seq[Symbol]
        for e in syms:
            if e.kind == skChoice:
                tmp.add e.syms
            else:
                tmp.add e
        tmp
    Symbol(kind: skChoice, syms: syms)
proc `$`*(self: Symbol): string =
    if self.kind == skChoice:
        let tmp = self.syms.join("\n").indent(2)
        &"SymChoice:\n{tmp}"
    else:
        fmt"Sym({self.kind}, type: {self.typ})"

# Id
proc hash*(self: Id): Hash =
    self.name.hash

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

proc newTypeSchemeId(self: TypeEnv): TypeSchemeId =
    result = self.typeschemeid
    inc self.typeschemeid

proc `$`*(self: Expr): string
proc `$`*(self: Statement): string
proc `$`*(self: Id): string =
    self.name
proc `$`*(self: ElifBranch): string =
    let tmp = (self.branch.map(`$`).join("\n")).indent(2)
    &"{self.cond}:\n{tmp}"
proc `$`*(self: Expr): string =
    case self.kind
    of ekCall:
        let tmp = self.params.join(", ")
        fmt"{self.callee}({tmp})"
    of ekBinOp:
        fmt"{self.lhs} {self.op} {self.rhs}"
    of ekIfExpr:
        "if " & self.elifbranches.map(`$`).join("\nelif ") & (if self.elsebranch.len == 0: "" else: "\nelse:\n" & self.elsebranch.map(`$`).join("\n").indent(2))
    of ekLoopExpr:
        let a = ($self.body).indent(2)
        &"loop:\n{a}"
    of ekBlockExpr:
        let a = ($self.body).indent(2)
        &"block:\n{a}"
    of ekChar:
        self.charval.repr
    of ekInt:
        $self.intval
    of ekFloat:
        $self.floatval
    of ekString:
        self.strval.repr
    of ekSym:
        self.name
proc `$`*(self: Pattern): string =
    case self.kind
    of pkDiscard:
        "_"
    of pkId:
        self.id.name
    of pkLiteral:
        $self.lit
    of pkPatterns:
        self.pats.join(", ")
proc `$`*(self: IdentDef): string =
    let
        typ = if self.typ.isNil: "" else: fmt": {self.typ}"
        default = if self.default.isNil: "" else: fmt" = {self.default}"
    fmt"{self.id}{typ}{default}"
proc `$`*(self: Statement): string =
    case self.kind
    of stkComment:
        fmt"# {self.comment}"
    of stkVarDecl:
        let tmp = self.iddefs.map(`$`).join("\n").indent(2)
        &"var\n{tmp}"
    of stkLetDecl:
        ""
    of stkConstDecl:
        ""
    of stkFuncDef:
        ""
    of stkTempDef:
        ""
    of stkMacroDef:
        ""
    of stkIterDef:
        ""
    of stkExprStmt:
        $self.exp
    of stkAsign:
        fmt"{self.lhs} = {self.rhs}"
proc `$`*(self: Program): string =
    self.stmts.map(`$`).join("\n")

proc typeInduction(self: Expr, env: TypeEnv): Type =
    case self.kind
    of ekCall:
        nil
    of ekBinOp:
        nil
    of ekIfExpr:
        nil
    of ekLoopExpr:
        nil
    of ekBlockExpr:
        nil
    of ekChar:
        Type(kind: tkCon, name: "char")
    of ekInt:
        Type(kind: tkCon, name: "int")
    of ekFloat:
        Type(kind: tkCon, name: "float")
    of ekString:
        Type(kind: tkCon, name: "string")
    of ekSym:
        let sym = env.symenv.env[self.name]
        # TODO: choice
        if sym.kind == skChoice:
            assert false
            nil
        else:
            sym.typ

proc registerId(self: IdentDef, kind: range[skFunc..skConst], env: TypeEnv) =
    var
        id = self.id
        typ = self.typ
        default = self.default
        sym = kind.newSymbol()
    if typ.isNil and default.isNil:
        assert false
    if not default.isNil:
        let rty = default.typeInduction(env)
        if typ.isNil:
            sym.typ = rty
        else:
            # TODO: cast
            let lty = typ.typeInduction(env)
            if lty == rty:
                sym.typ = lty
            else:
                assert false
    case id.kind
    of pkId:
        id.id.sym = sym
        # TODO: search in parent
        env.symenv.env[id.id.name] = sym
    else:
        # TODO: more patterns
        discard

proc typeInduction(self: Statement, env: TypeEnv): Type =
    case self.kind
    of stkComment:
        newNoneType()
    of stkVarDecl:
        for d in self.iddefs:
            d.registerId(skVar, env)
        nil
    of stkLetDecl:
        nil
    of stkConstDecl:
        nil
    of stkFuncDef:
        nil
    of stkTempDef:
        nil
    of stkMacroDef:
        nil
    of stkIterDef:
        nil
    of stkExprStmt:
        self.exp.typeInduction(env)
    of stkAsign:
        nil

proc typeInduction(self: Program, env: TypeEnv)=
    for e in self.stmts:
        discard e.typeInduction(env)

proc toExpr(self: AstNode): Expr
proc toStatement(self: AstNode): Statement
proc toStmtList(self: AstNode): StmtList
proc toId(self: AstNode): Id =
    assert self.kind == akId
    Id(lineInfo: self.lineInfo, name: self.strVal)
proc toSymbol(self: AstNode): Symbol =
    # TODO:
    nil
proc toElifBranch(self: AstNode): ElifBranch =
    assert self.kind == akElifBranch
    ElifBranch(cond: self.children[0].toExpr(), branch: self.children[1].toStmtList())
proc toElseBranch(self: AstNode): StmtList =
    assert self.kind == akElseBranch
    self.children[0].toStmtList()
proc toExpr(self: AstNode): Expr =
    case self.kind
    of akFailed, akEmpty, akComment, akStmtList, akStatement:
        assert false
        nil
    of akBlockExpr:
        nil
    of akIfExpr:
        let (elifb, elseb) = if self.children[^1].kind == akElseBranch:
            (self.children[0..^2].map(toElifBranch), self.children[^1].toElseBranch())
        else:
            (self.children.map(toElifBranch), @[])
        Expr(lineInfo: self.lineInfo, kind: ekIfExpr, elifbranches: elifb, elsebranch: elseb)
    of akWhenExpr:
        nil
    of akElifBranch:
        nil
    of akElseBranch:
        nil
    of akLoopExpr:
        nil
    of akInfix:
        nil
    of akPrefix:
        nil
    of akPostfix:
        nil
    of akDot:
        nil
    of akCommand:
        Expr(lineInfo: self.lineInfo, kind: ekCall, callee: self.children[0].toExpr(), params: self.children[1..^1].map(toExpr))
    of akCall:
        Expr(lineInfo: self.lineInfo, kind: ekCall, callee: self.children[0].toExpr(), params: self.children[1..^1].map(toExpr))
    of akChar:
        Expr(lineInfo: self.lineInfo, kind: ekChar, charval: chr self.intVal)
    of akInt:
        Expr(lineInfo: self.lineInfo, kind: ekInt, intval: self.intVal)
    of akFloat:
        Expr(lineInfo: self.lineInfo, kind: ekFloat, floatval: self.floatVal)
    of akString:
        Expr(lineInfo: self.lineInfo, kind: ekString, strval: self.strVal)
    of akId:
        Expr(lineInfo: self.lineInfo, kind: ekSym, name: self.strVal, sym: self.toSymbol())
    of akPat:
        nil
proc toPattern(self: AstNode): Pattern =
    case self.kind:
    of akFailed, akEmpty, akComment, akStmtList, akStatement, akExpr:
        assert false
        nil
    of akDiscardPattern:
        Pattern(kind: pkDiscard)
    of akIdPattern:
        Pattern(kind: pkId, id: self.children[0].toId())
    of akLiteralPattern:
        Pattern(kind: pkLiteral)
    of akPatterns:
        if self.children.len == 1:
            self.children[0].toPattern()
        else:
            Pattern(kind: pkPatterns, pats: self.children.map(toPattern))
proc toIdentDef(self: AstNode): IdentDef =
    assert self.kind == akIdentDef
    assert self.children.len == 3
    let
        pat = self.children[0]
        typ = self.children[1]
        default = self.children[2]
    IdentDef(id: pat.toPattern(), default: default.toExpr())
proc toStatement(self: AstNode): Statement =
    case self.kind
    of akFailed:
        assert false
        nil
    of akEmpty:
        assert false
        nil
    of akComment:
        Statement(lineInfo:self.lineInfo, kind: stkComment, comment: self.children[0].strVal)
    of akStmtList:
        assert false
        nil
    of akLetSection:
        Statement(lineInfo: self.lineInfo, kind: stkLetDecl, iddefs: self.children.map(toIdentDef))
    of akVarSection:
        Statement(lineInfo: self.lineInfo, kind: stkVarDecl, iddefs: self.children.map(toIdentDef))
    of akConstSection:
        Statement(lineInfo: self.lineInfo, kind: stkConstDecl, iddefs: self.children.map(toIdentDef))
    of akFuncDef:
        nil
    of akTempDef:
        nil
    of akMacroDef:
        nil
    of akIterDef:
        nil
    of akIdentDef:
        nil
    of akAsign:
        nil
    of akExpr:
        Statement(lineInfo: self.lineInfo, kind: stkExprStmt, exp: self.toExpr())
    of akPat:
        nil
proc toStmtList(self: AstNode): StmtList =
    case self.kind
    of akStmtList:
            self.children.map toStatement
    else:
        @[]
proc flatten(self: AstNode) =
    if self.kind == akStmtList:
        var ch: seq[AstNode] = @[]
        for e in self.children:
            if e.kind == akStmtList:
                ch.add e.children
            else:
                ch.add e
        self.children = ch
    if self.kind in akHasChildren:
        for e in self.children:
            e.flatten()
proc toProgram(self: AstNode): Program =
    self.flatten()
    case self.kind
    of akStmtList:
        Program(
            stmts: self.toStmtList()
        )
    else:
        nil


proc sema*(node: AstNode, env: Environment): Program =
    # registerSymbol(node, env)
    # simpleTyping(node, env)
    # resolveTyping(node, env)
    let
        tenv = newTypeEnv()
        program = node.toProgram
    program.typeInduction(tenv)
    program