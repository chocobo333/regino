
import ast
import parsers
import sema
# import codegen

proc compile(filename: string): string =
    var
        parser = newParser()
        tmp =  parser.parse(filename)
        parsed_string = parser.parse(filename)
    
    echo parsed_string

    echo sema(parsed_string)

when isMainModule:
    echo compile("test/test01.rgn")