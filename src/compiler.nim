
import os

import codegen
import errors
import projects

import il


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

proc sema*(filename: seq[string]) = 
    var
        filename = filename[0].absolutePath
        module = newModule()
        project = newProject(filename)
    project.parse
    project.sema

proc compile*(filename: seq[string]): int =
    var
        filename = filename[0].absolutePath
        module = newModule()
        project = newProject(filename)
    project.parse
    project.sema
    let program = project.mainProgram
    if project.errExists:
        project.echoErrs
        return 1
    program.codegen(module, true)
    for e in module.linkModules:
        discard module.module.link(e)
    # module = module.optimize
    let f = open(filename.absolutePath.splitPath.head / "test.ll", fmWrite)
    f.write($module.module)
    defer:
        close f
    0

when isMainModule:
    # discard compile(@["test/ptr.rgn"])
    sema(@["test/unit.rgn"])
