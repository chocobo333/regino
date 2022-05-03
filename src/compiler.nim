
import os

import parsers
import sema
import codegen
import errors
import projects


from llvm import `$`, link

import llvm/raw as llraw

proc optimize(self: Module): Module =
    let
        module = self.module.module
        fpm = createFunctionPassManagerForModule(module)
        mpm = createPassManager()
        pmb = passManagerBuilderCreate()
    pmb.passManagerBuilderSetOptLevel(2)
    pmb.passManagerBuilderUseInlinerWithThreshold(255)
    pmb.passManagerBuilderPopulateModulePassManager(mpm)
    pmb.passManagerBuilderPopulateFunctionPassManager(fpm)
    pmb.passManagerBuilderDispose()

    discard fpm.initializeFunctionPassManager()
    var fn = module.getFirstFunction()
    while not fn.isNil:
        discard fpm.runFunctionPassManager(fn)
        fn = fn.getNextFunction()
    discard fpm.finalizeFunctionPassManager()
    discard mpm.runPassManager(module)
    fpm.disposePassManager()
    mpm.disposePassManager()

    self.module.module = module
    self

proc compile*(filename: string) =
    var
        module = newModule()
        project = newProject(filename)
        program = project.parse
        perrs = project.perrs
        terrs = project.terrs
    if perrs.len == 0 and terrs.len == 0:
        program.codegen(module, true)
    else:
        for e in perrs:
            echo e
        for e in terrs:
            echo e
    for e in module.linkModules:
        discard module.module.link(e)
    # module = module.optimize
    let f = open(filename.absolutePath.splitPath.head / "test.ll", fmWrite)
    f.write($module.module)
    defer:
        close f

when isMainModule:
    compile("test/test04.rgn")
