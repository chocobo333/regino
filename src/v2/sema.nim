
import options

import il
import typeenv
import errors
import utils

import sema/[
    scopes,
    eval,
]


proc sema*(self: Program): seq[TypeError] =
    echo self.get(65).fn.param.rety.get.treeRepr
    let
        mainScope = self.setScope()
        env = newTypeEnv(mainScope)
        rety = self.infer(env)
    self.check(env)
    debug env.errs
    # debug mainScope
    debug self.eval(env)
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
    # echo program.scope
    echo program.stmts[^2].iddefs[0].default.get.typ
    echo program.stmts[^2].iddefs[0].default.get.callee.typ
    echo program.stmts[^2].iddefs[0].default.get.callee.kind
    echo program.stmts[^2].iddefs[0].default.get.callee.typ.symbol
