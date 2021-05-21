
import ast
import parsers
import sema
import codegen

import sema/[
    convert,
    infer
]

proc compile(filename: string): string =
    var
        module = newModule()
        parser = newParser()

    echo parser.parse(filename).sema(module)

when isMainModule:
    echo compile("test/test.rgn")