
from os import `/`, splitPath, absolutePath
import options

import ast
import lineInfos

import rts/[
    il,
    types,
    symbols,
    infer,
    typeenvs
]
import sema/[
    convert
]

import codegen

import llvm except Type, Module


proc link(self: Metadata, module: Module) =
    let param = self.param
    assert param.kind == TermKind.String
    let
        path = param.lineInfo.fileId.getFileInfo.filename.splitPath.head.absolutePath / param.s
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

proc globalMetadada(self: Term, module: Module) =
    assert self.kind == TermKind.Seq
    for e in self.ts:
        # TODO: remove it
        if e.isNil:
            continue
        if e.kind == TermKind.Metadata:
            let metadata = e.metadata
            case metadata.kind
            of MetadataKind.Link:
                metadata.link(module)
            else:
                assert false

proc sema*(node: AstNode, module: Module): Term =
    var
        tenv = newTypeEnv()
        program = newTerm(node)
        rety = program.typeInfer(tenv)
    program.globalMetadada(module)
    let
        tmp = case rety.kind
        of types.TypeKind.Unit:
            Term.Unit
        of types.TypeKind.Int:
            Term.TypeOf(Term.Int(0))
        else:
            assert rety.kind notin @[types.TypeKind.Unit, types.TypeKind.Int]
            nil
        main = newFunction("main", @[], tmp, program)
        sym = Symbol(typ: Type.Arr(@[], rety))
    tmp.typ = Type.TypeDesc(rety)
    main.id.symbol = sym
    main.id.typ = Type.Arr(@[], rety)
    Term.FuncDef(main)