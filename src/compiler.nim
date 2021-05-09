
import ast
import parsers
import sema
import codegen

proc compile(filename: string): string =
    var
        parser = newParser()
        env = newEnvironment()
        tmp =  parser.parse(filename)
    parser.parse(filename).sema(env).codegen()

when isMainModule:
    echo compile("test/test01.rgn")