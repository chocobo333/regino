
import strutils
import strformat
import sequtils

import types
import ../lineInfos


type
    Program* = ref object
        stmts*: StmtList

    StmtList* = seq[Statement]
    StatementKind* = enum
        stkComment
        stkVarDecl
        stkLetDecl
        stkConstDecl
        stkAliasDecl
        stkFuncDef
        stkTempDef
        stkMacroDef
        stkIterDef
        stkExprStmt
        stkAsign
    Statement* = ref object
        lineInfo*: LineInfo
        case kind*: StatementKind
        of stkComment:
            comment*: string
        of stkVarDecl, stkLetDecl, stkConstDecl, stkAliasDecl:
            iddefs*: seq[IdentDef]
        of stkFuncDef, stkTempDef, stkMacroDef, stkIterDef:
            fn*: Function
        of stkExprStmt:
            exp*: Expr
        of stkAsign:
            lhs*: Pattern
            rhs*: Expr
    Function* = ref object
        name*: string
        rety*: Type
        paramty*: seq[Type]
        params*: seq[IdentDef]
        body*: Expr
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
        ekBool
        ekId
    Expr* = ref object
        lineInfo*: LineInfo
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
        of ekBool:
            boolval*: bool
        of ekId:
            name*: string
            id*: Id
    ElifBranch* = ref object
        cond*: Expr
        branch*: StmtList


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
    of ekBool:
        self.boolval.repr
    of ekId:
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
    of stkAliasDecl:
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
