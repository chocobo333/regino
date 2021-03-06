
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
        akDiscard
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
        akBracketExpr
        akCommand
        akCall
        akTuple
        akRecord
        akUs
        akChar
        akInt
        akFloat
        akBool
        akString
        akId
        akComment
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

type
    AstNode* = ref object
        loc*: Location
        case kind*: AstKind
        of akChar:
            charVal*: char
        of akInt:
            intVal*: BiggestInt
            bits*: uint
        of akFloat:
            floatVal*: BiggestFloat
        of akBool:
            boolVal*: bool
        of akString..akId:
            strVal*: string
        of akComment:
            comments*: seq[string]
        else:
            children*: seq[AstNode]

proc `$`*(self: AstNode): string =
    let k = fmt"{($self.kind)[2..^1].green} {""@"".cyan} {($self.loc)}"
    case self.kind:
    of akChar:
        genGraph(k, self.charVal)
    of akInt:
        genGraph(k, self.intVal)
    of akFloat:
        genGraph(k, self.floatVal)
    of akBool:
        genGraph(k, self.boolVal)
    of akString..akId:
        genGraph(k, &"\"{self.strVal}\"")
    of akComment:
        genGraphS(k, self.comments)
    else:
        genGraphS(k, self.children)

proc newCharNode*(val: char, loc: Location = newLocation()): AstNode =
    AstNode(kind: akChar, charval: val, loc: loc)
proc newIntNode*(val: BiggestInt, bits: uint, loc: Location = newLocation()): AstNode =
    AstNode(kind: akInt, intVal: val, bits: bits, loc: loc)

proc newFloatNode*(val: BiggestFloat, loc: Location = newLocation()): AstNode =
    AstNode(kind: akFloat, floatVal: val, loc: loc)

proc newBoolNode*(val: bool, loc: Location = newLocation()): AstNode =
    AstNode(kind: akBool, boolVal: val, loc: loc)

proc newStrNode*(val: string, loc: Location = newLocation()): AstNode =
    AstNode(kind: akString, strVal: val, loc: loc)

proc newIdNode*(name: string, loc: Location = newLocation()): AstNode =
    AstNode(kind: akId, strVal: name, loc: loc)
proc newCommentNode*(comments: seq[string], loc: Location = newLocation()): AstNode =
    AstNode(kind: akComment, comments: comments, loc: loc)

proc newTreeNode*(kind: range[akFailed..akRecord], children: seq[AstNode], loc: Location = newLocation(children[0].loc.uri, children[0].loc.`range`.a, children[^1].loc.`range`.b)): AstNode =
    AstNode(kind: kind, children: children, loc: loc)

proc newNode*(kind: AstKind, loc: Location = newLocation()): AstNode =
    AstNode(kind: kind, loc: loc)

proc newEmptyNode*(loc: Location = newLocation()): AstNode = akEmpty.newNode(loc)
proc newFailedNode*(loc: Location = newLocation()): AstNode = akFailed.newNode(loc)
proc empty*(t: typedesc[AstNode]): AstNode = newEmptyNode()

proc isEmpty*(self: AstNode): bool =
    self.kind == akEmpty

proc seta*(self: AstNode, a: spanned.Position): AstNode =
    result = self
    result.loc.`range`.a = a

proc setb*(self: AstNode, b: spanned.Position): AstNode =
    result = self
    result.loc.`range`.b = b

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
    of akDiscard:
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
    of akBracketExpr:
        $self.children[0] & "[" & self.children[1..^1].join(", ") & "]"
    of akCommand:
        var
            args = self.children[1..^1].map(repr2).join(", ")
        &"{self.children[0].repr} {args}"
    of akUs:
        "_"
    of akCall:
        ""
    of akTuple:
        "(" & self.children.join(", ") & ")"
    of akRecord:
        "(" & self.children.join(", ") & ")"
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
