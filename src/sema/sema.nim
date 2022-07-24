
import tables

import ir
import projects
import typeenvs
import infer
import eval
import check
import resolve

import ../utils


proc sema*(self: Project) =
    let
        main = self.programs[self.main]
    self.env = newTypeEnv(main.scope)
    main.predeclare(self, global=true)
    discard main.preeval(self, global=true)
    debug self.env.constraints
    self.env.resolveEq
    main.infer(self, global=true)
    discard main.check(self, global=true)
    debug main


when isMainModule:
    import ../lineinfos
    let
        project = newProject("test.rgn")
    project.programs[project.main] = Expression.Lit(Literal.Integer(3, 0), newLocation())
    project.sema
    echo project.programs[project.main]
