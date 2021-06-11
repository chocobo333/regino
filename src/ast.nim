
import strformat
import strutils
import sequtils

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
        akMetadata
        akParams
        akBlockExpr
        akIfExpr
        akWhenExpr
        akElifBranch
        akElseBranch
        akLoopExpr
        akLambdaDef
        akInfix
        akPrefix
        akPostfix
        akDot
        akCommand
        akCall
        akDiscardPattern
        akIdPattern
        akLiteralPattern
        akPatterns
        akChar
        akInt
        akFloat
        akBool
        akString
        akId
    # DefKind = enum
    #     Func
    #     Template
    #     Macro
    #     Iterator

const
    akLiteral* = {akChar..akString}
    akHasChildren* = {akStmtList..akCall}
    akStatement* = {akLetSection..akAsign}
    akExpr* = {akBlockExpr..akCommand, akCall..akId}
    akPat* = {akDiscardPattern..akPatterns}

type
    AstNode* = ref object
        lineInfo*: LineInfo
        case kind*: AstKind
        of akChar..akInt:
            intVal*: BiggestInt
        of akFloat:
            floatVal*: BiggestFloat
        of akBool:
            boolVal*: bool
        of akString..akId:
            strVal*: string
        else:
            children*: seq[AstNode]

proc `$`*(self: AstNode): string =
    let k = fmt"{($self.kind)[2..^1].green} {""@"".cyan} {($self.lineInfo)}"
    case self.kind:
    of akChar..akInt:
        genGraph(k, self.intVal)
    of akFloat:
        genGraph(k, self.floatVal)
    of akBool:
        genGraph(k, self.boolVal)
    of akString..akId:
        genGraph(k, &"\"{self.strVal}\"")
    else:
        genGraphS(k, self.children)

proc newIntNode*(val: BiggestInt, info: LineInfo = newLineInfo(-1, newPosition(), newPosition())): AstNode =
    AstNode(kind: akInt, intVal: val, lineInfo: info)

proc newFloatNode*(val: BiggestFloat, info: LineInfo = newLineInfo(-1, newPosition(), newPosition())): AstNode =
    AstNode(kind: akFloat, floatVal: val, lineInfo: info)

proc newBoolNode*(val: bool, info: LineInfo = newLineInfo(-1, newPosition(), newPosition())): AstNode =
    AstNode(kind: akBool, boolVal: val, lineInfo: info)

proc newStrNode*(val: string, info: LineInfo = newLineInfo(-1, newPosition(), newPosition())): AstNode =
    AstNode(kind: akString, strVal: val, lineInfo: info)

proc newIdNode*(name: string, info: LineInfo = newLineInfo(-1, newPosition(), newPosition())): AstNode =
    AstNode(kind: akId, strVal: name, lineInfo: info)

proc newTreeNode*(kind: range[akFailed..akPatterns], children: seq[AstNode], info: LineInfo = newLineInfo(children[0].lineInfo.fileid, children[0].lineInfo.span.a, children[^1].lineInfo.span.b)): AstNode =
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
    of akMetadata:
        ""
    of akParams:
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
    of akLambdaDef:
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
    of akBool:
        $self.boolVal
    of akString:
        # "\"" & self.strVal & "\""
        self.strVal.repr
    of akId:
        self.strVal
