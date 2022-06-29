
import il
import typeenv
import errors
import utils

import sema/[
    scopes,
    eval,
]


proc sema*(self: Program): seq[TypeError] =
    let
        mainScope = self.setScope()
        env = newTypeEnv(mainScope)
        rety = self.infer(env)
    self.check(env)
    debug env.errs
    # debug mainScope
    # evalするとtypがおかしくなる
    # debug self.eval(env)
    env.errs


when isMainModule:
    import parsers
    import options
    import desugar
    let
        f = open("test/test05.rgn")
        s = f.readAll
        program = Program(Source.from(s)).get.desugar


    let
        errs = program.sema
    f.close
    # debug program
