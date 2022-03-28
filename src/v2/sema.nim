
import il
import typeenv

import sema/[
    scopes,
    eval,
]


proc sema*(self: Program) =
    let
        mainScope = self.setScope()
        env = newTypeEnv(mainScope)
    echo self.infer(env)
    echo mainScope


when isMainModule:
    import parsers
    import options
    let
        f = open("test/test.rgn")
        s = f.readAll
        program = Program(Source.from(s)).get
    f.close

    echo program

    program.sema
