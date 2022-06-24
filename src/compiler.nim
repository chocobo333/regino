
import os

import codegen
import errors
import projects
import tables


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

proc compile*(filename: seq[string]): int =
    var
        filename = filename[0]
        module = newModule()
        project = newProject(filename)
    project.parse
    project.sema
    let program = project.program
    if project.perrs.len == 0 and project.terrs.len == 0:
        program.codegen(module, true)
    else:
        project.echoErrs
        return 1
    for e in module.linkModules:
        discard module.module.link(e)
    # module = module.optimize
    let f = open(filename.absolutePath.splitPath.head / "test.ll", fmWrite)
    f.write($module.module)
    defer:
        close f
    0

when isMainModule:
    discard compile(@["test/test.rgn"])
