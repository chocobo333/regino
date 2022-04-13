
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
    echo self.eval(env)
    env.errs


when isMainModule:
    import parsers
    import options
    let
        f = open("test/test05.rgn")
        s = f.readAll
        program = Program(Source.from(s)).get
        errs = program.sema
    f.close
    # echo program.scope
    echo program.stmts[^2].iddefs[0].default.get.typ
    echo program.stmts[^2].iddefs[0].default.get.callee.typ
    echo program.stmts[^2].iddefs[0].default.get.callee.kind
    echo program.stmts[^2].iddefs[0].default.get.callee.typ.symbol
