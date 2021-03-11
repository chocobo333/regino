
import strformat

import lineInfos

import coloredString

import parserCombinator/utils


type
    AstKind* = enum
        akFailed
        akEmpty
        akStmtList
        akLetSection
        akVarSection
        akConstSection
        # akDefinition
        akFuncDef
        akTempDef
        akMacroDef
        akIterDef
        akIdentDef
        akAsign
        akBlockExpr
        akIfExpr
        akElifBlock
        akElseBlock
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

const
    akLiteral = {akInt..akString}

type
    AstNode* = object
        lineInfo*: LineInfo
        case kind*: AstKind
        of akChar..akInt:
            intVal*: BiggestInt
        of akFloat:
            floatVal*: BiggestFloat
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
    of akString..akId:
        genGraph(k, &"\"{self.strVal}\"")
    else:
        genGraphS(k, self.children)


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

proc seta*(self: AstNode, a: Position): AstNode =
    result = self
    result.lineInfo.span.a = a

proc setb*(self: AstNode, b: Position): AstNode =
    result = self
    result.lineInfo.span.b = b