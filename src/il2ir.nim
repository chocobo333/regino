
import sequtils
import tables

import syntax/il
import syntax/projects as ilprojects
import sema/projects as irprojects
import sema/ir/[
    ir, 
    constructors
]

import lineinfos

proc il2ir*(self: Statement): ir.Expression =
    # TODO:
    discard

proc il2ir*(self: Program): ir.Expression =
    ir.Expression.Seq(
        self.stmts.map(il2ir), 
        newScope(),
        # TODO:
        # calculate location from stmts or 
        # change definition of Program to have location and revise parser of Program
        newLocation()
    )

proc il2ir*(self: ilprojects.Project): irprojects.Project =
    result = irprojects.newProject(self.main)
    for uri, program in self.program.pairs:
        result.programs[uri] = program.il2ir