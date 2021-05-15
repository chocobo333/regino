
import tables
import hashes
import sequtils
import strutils
import strformat
from os import `/`, splitPath, absolutePath
import options

import ast
import lineInfos

import sema/types
import sema/ir

import codegen

import llvm except Type, Module

export `$`

# Type
proc `$`*(self: Type): string =
    case self.kind
    of tkNone:
        "none"
    of tkUnit:
        "Unit"
    of tkChar:
        "char"
    of tkInt:
        "int"
    of tkFloat:
        "float"
    of tkString:
        "string"
    of tkBool:
        "bool"
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
        Type(kind: tkChar)
    of ekInt:
        Type(kind: tkInt)
    of ekFloat:
        Type(kind: tkFloat)
    of ekString:
        Type(kind: tkString)
    of ekBool:
        Type(kind: tkBool)
    of ekId:
        nil
        # let sym = env.symenv.env[self.name]
        # # TODO: choice
        # if sym.kind == skChoice:
        #     assert false
        #     nil
        # else:
        #     sym.typ

# proc registerId(self: IdentDef, kind: range[skFunc..skConst], env: TypeEnv) =
#     var
#         id = self.id
#         typ = self.typ
#         default = self.default
#         sym = kind.newSymbol()
#     if typ.isNil and default.isNil:
#         assert false
#     if not default.isNil:
#         let rty = default.typeInduction(env)
#         if typ.isNil:
#             sym.typ = rty
#         else:
#             # TODO: cast
#             let lty = typ.typeInduction(env)
#             if lty == rty:
#                 sym.typ = lty
#             else:
#                 assert false
#     case id.kind
#     of pkId:
#         id.id.sym = sym
#         # TODO: search in parent
#         env.symenv.env[id.id.name] = sym
#     else:
#         # TODO: more patterns
#         discard


proc typeInduction(self: Statement, env: TypeEnv): Type
proc typeInduction(self: StmtList, env: TypeEnv): Type =
    for e in self:
        result = e.typeInduction(env)
proc typeInduction(self: Statement, env: TypeEnv): Type =
    echo "in typeInduction for statement"
    case self.kind
    of stkComment:
        newNoneType()
    of stkVarDecl:
        # for d in self.iddefs:
        #     d.registerId(skVar, env)
        nil
    of stkLetDecl:
        nil
    of stkConstDecl:
        nil
    of stkAliasDecl:
        nil
    of stkFuncDef:
        nil
        # echo "in function def"
        # var 
        #     fn = self.fn
        #     fnType = Type(kind: tkFunc, rety: fn.rety, paramty: fn.paramty)
        # extend(env, fn.name, fnType)
        # var localEnv = addScope(env)
        # if fn.rety != typeInduction(fn.body, localEnv):
        #     # TODO: raise ERROR 
        #     # return type dosent match body type
        #     return newNoneType()
        # echo "out function def"
        # return fnType
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
    of stkMetadata:
        nil

proc typeInduction(self: Program, env: TypeEnv)=
    echo "in typeinduction for program"
    echo self.stmts
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
    of akFailed, akEmpty, akComment, akStmtList, akStatement, akParams:
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
    of akBool:
        Expr(lineInfo: self.lineInfo, kind: ekBool, boolval: self.boolVal)
    of akId:
        Expr(lineInfo: self.lineInfo, kind: ekId, name: self.strVal)
    of akPat:
        nil
    of akMetadata:
        nil
proc toPattern(self: AstNode): Pattern =
    case self.kind:
    of akFailed, akEmpty, akComment, akStmtList, akStatement, akExpr, akParams:
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
    of akMetadata:
        nil
proc toIdentDef(self: AstNode): IdentDef =
    assert self.kind == akIdentDef
    assert self.children.len == 3
    let
        pat = self.children[0]
        typ = self.children[1]
        default = self.children[2]
    IdentDef(id: pat.toPattern(), default: default.toExpr())
proc toMetadata(self: AstNode): Metadata =
    assert self.kind == akMetadata
    assert self.children[0].kind == akId
    var param = 
        if len(self.children) == 1:
            nil
        else:
            self.children[1].toExpr()
    Metadata(name: self.children[0].strVal, param: param)
proc toStatement(self: AstNode): Statement =
    echo "in toStatement"
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
    of akAliasSection:
        Statement(lineInfo: self.lineInfo, kind: stkAliasDecl, iddefs: self.children.map(toIdentDef))
    of akFuncDef:
        assert self.children[0].kind == akId
        let
            name = self.children[0].strVal
            funty = self.children[1]
            metadata = self.children[2].toMetadata()
            body = self.children[3].toStmtList()
            function = Function(name: name, rety: toExpr(funty.children[0]), params: funty.children[1].children.map(toIdentDef), metadata: metadata, body: body)
        Statement(lineInfo: self.lineInfo, kind: stkFuncDef, fn: function)
        # var
            # function = Function(name: self.children[0].strVal, rety: newNoneType(), paramty: nil, params: self.children[1].children.map(toIdentDef),)
        # var identDef = self.child
        # Statement(lineInfo: self.lineInfo, kind: stkFuncDef, fn: )
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
    of akParams:
        nil
    of akExpr:
        Statement(lineInfo: self.lineInfo, kind: stkExprStmt, exp: self.toExpr())
    of akPat:
        nil
    of akMetadata:
        let metadata = Metadata(name: self.children[0].toExpr.name, param: self.children[1].toExpr)
        Statement(lineInfo: self.lineInfo, kind: stkMetadata, metadata: metadata)
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

proc link(self: Metadata, module: Module) =
    let param = self.param
    assert param.kind == ekString
    let
        path = param.lineInfo.fileId.getFileInfo.filename.splitPath.head.absolutePath / param.strval
        f = open(path)
        s = f.readAll()
        module2 = llvm.parseIr(module.cxt, path)
    if module2.isSome:
        let m = module2.get
        module.linkModules.add m
        for fn in m.funcs:
            module.linkFuncs.add fn
    else:
        # TODO: err msg
        assert false
    f.close()
proc globalMetada(self: Program, module: Module) =
    for e in self.stmts:
        # TODO: remove it
        if e.isNil:
            continue
        if e.kind == stkMetadata:
            let metadata = e.metadata
            case metadata.name
            of "link":
                metadata.link(module)
            else:
                assert false

proc sema*(node: AstNode, module: Module): Program =
    # registerSymbol(node, env)
    # simpleTyping(node, env)
    # resolveTyping(node, env)
    let
        tenv = newTypeEnv()
        program = node.toProgram
    program.globalMetada(module)
    program.typeInduction(tenv)
    program