
import tables

import ir
import projects
import infer
import eval
import check


proc sema*(self: Project) =
    let
        main = self.programs[self.main]
    discard main.preeval(self)
    discard main.infer(self)
    discard main.check(self)
