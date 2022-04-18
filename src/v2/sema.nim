
import options

import il
import typeenv
import errors
import utils

import sema/[
    scopes,
    eval,
    region
]


proc sema*(self: Program): seq[TypeError] =
    let
        mainScope = self.setScope()
        env = newTypeEnv(mainScope)
        rety = self.infer(env)
    self.check(env)
    debug env.errs
    debug mainScope
    # debug self.eval(env)
    env.errs


when isMainModule:
    import parsers
    import options
    let
        f = open("test/test.rgn")
        s = f.readAll
        program = Program(Source.from(s)).get
        errs = program.sema
    f.close
    # debug program.scope
