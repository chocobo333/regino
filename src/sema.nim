
from os import `/`, splitPath, absolutePath
import options

import ast
import lineInfos

import
    infer,
    il,
    typeenvs
import convert

import codegen

import llvm except Value, Type, Module


proc link(self: Metadata, module: Module) =
    let param = self.param
    assert param.kind == TermKind.String
    let
        path = param.loc.uri.path.splitPath.head.absolutePath / param.strval
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

proc globalMetadada(self: ref Term, module: Module) =
    assert self.kind == TermKind.Seq
    for e in self.terms:
        # TODO: remove it
        if e.isNil:
            continue
        if e.kind == TermKind.Meta:
            let metadata = e.metadata
            case metadata.kind
            of MetadataKind.Link:
                metadata.link(module)
            else:
                assert false

proc sema*(node: AstNode, module: Module): ref Term =
    var
        mainScope = newScope(nil)
        tenv = newTypeEnv(mainScope)
        program = newTerm(node, mainScope)
        rety = program.infer(tenv, true)
    program.globalMetadada(module)
    let
        tmp = case rety.kind
        of il.ValueKind.Unit:
            Term.unit
        of il.ValueKind.Integer:
            Term.TypeOf(Term.Integer(0))
        else:
            assert rety.kind notin @[il.ValueKind.Unit, il.ValueKind.Integer]
            nil
        mainid = newIdent("main")
        main = Term.Funcdef(newFunction(mainid, @[], tmp, newBody(program, mainScope)))
        mainty = Value.Pi(@[], @[], rety)
        sym = Symbol.Func(mainid, mainty, main, true)
    tenv.addIdent(mainid, sym)
    mainid.typ = mainty
    mainid.typ.symbol = some sym
    # mainid.typ.symbol.get.instances[mainid.typ] = Symbol()
    echo program
    # echo mainScope
    main
