
import tables

import ir
import projects
import infer
import eval
import check
import resolve


proc sema*(self: Project) =
    let
        main = self.programs[self.main]
    main.predeclare(self, global=true)
    discard main.preeval(self)
    discard main.infer(self)
    discard main.check(self)


when isMainModule:
    import ../lineinfos
    let
        project = newProject("test.rgn")
    project.programs[project.main] = Expression.Lit(Literal.Integer(3, 0), newLocation())
    project.sema
    echo project.programs[project.main]
