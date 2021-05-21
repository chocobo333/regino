
import tables

import sema/types
import sema/il

import llvm


type
    Module* = ref object
        module*: llvm.Module
        cxt*: Context
        curFun*: Value
        curBuilder*: Builder
        type2llvmType*: Table[types.Type, llvm.Type]
        linkModules*: seq[llvm.Module]
        linkFuncs*: seq[llvm.Value]

proc newModule*(name: string = "main"): Module =
    let
        cxt = newContext()
    Module(
        module: llvm.newModule(name, cxt),
        cxt: cxt,
        curBuilder: newBuilder(cxt),
        type2llvmType: initTable[types.Type, llvm.Type]()
    )

proc codegen2*(self: Term, module: Module) =
    discard
proc codegen*(self: Term): string =
    var
        cxt = newContext()
        module = newModule()
        i32 = intType(module.cxt, 32)
        mainty = functionType(i32, @[])
        main = module.module.addFunction("main", mainty)
        bb = main.appendBasicBlock("entry", cxt)
    module.curFun = main
    module.curBuilder.atEndOf(bb)
    self.codegen2(module)
    echo module.module