
import il
import typeenv
import errors
import utils

import sema/[
    scopes,
    eval,
    infer
]
import projects/projects


proc sema*(self: Program, project: Project): seq[TypeError] =
    let
        mainScope = self.setScope()
        env = newTypeEnv(mainScope)
        rety = self.infer(env, project)
    self.check(env, project)
    debug env.errs
    # debug mainScope
    # evalするとtypがおかしくなる
    # debug self.eval(env)
    env.errs


when isMainModule:
    import parsers
    import options
    let
        f = open("test/test.rgn")
        s = f.readAll
        program = Program(Source.from(s)).get
    # let
    #     errs = program.sema
    f.close
    debug program
