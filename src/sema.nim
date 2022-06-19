
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
    debug mainScope
    # evalするとtypがおかしくなる
    # debug self.eval(env)
    env.errs


when isMainModule:
    import parsers
    import options
    import desugar
    let
        f = open("test/unit.rgn")
        s = f.readAll
        program = Program(Source.from(s)).get
    
    let target = program.stmts[4].typedefSection.typedefs[0]
    debug target                                  # int = typeof(0)
    debug target.typ.expression.kind              # Call
    debug target.typ.expression.args              # @[ 0 ]

    let 
        desugaredProgram = program.desugar
        desugaredTarget = desugaredProgram.stmts[4].typedefSection.typedefs[0]
    debug desugaredTarget                         # int = typeof(0)
    debug desugaredTarget.typ.expression.kind     # Typeof
    debug desugaredTarget.typ.expression.`typeof` # 0

    let
        errs = desugaredProgram.sema
    f.close
    debug program
