
import os

import ast
import parsers
import sema
import codegen

import sema/[
    convert,
    infer
]

from llvm import `$`, link

proc compile(filename: string) =
    var
        module = newModule()
        parser = newParser()

    discard parser.parse(filename).sema(module).codegen(module)
    for e in module.linkModules:
        discard module.module.link(e)
    let f = open(filename.absolutePath.splitPath.head / "test.ll", fmWrite)
    f.write($module.module)
    defer:
        close f

when isMainModule:
    compile("test/test.rgn")