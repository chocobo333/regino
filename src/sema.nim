
import tables
from os import `/`, splitPath, absolutePath
import options

import ast
import lineInfos

import sema/[
    il,
    infer,
    convert
]

import codegen

import llvm except Type, Module


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
proc globalMetada(self: AstNode, module: Module) =
    assert self.kind == akStmtList
    for e in self.children:
        # TODO: remove it
        if e.isNil:
            continue
        if e.kind == akMetadata:
            let metadata = newMetadata(e)
            case metadata.name
            of "link":
                metadata.link(module)
            else:
                assert false

proc sema*(node: AstNode, module: Module): Term =
    # registerSymbol(node, env)
    # simpleTyping(node, env)
    # resolveTyping(node, env)
    var
        tenv = newTypeEnv()
        program = newTerm(node)
    # program.globalMetada(module)
    # program.typeInduction(tenv)
        rety = program.typeInfer(tenv)
    echo tenv.scope.vars
    program