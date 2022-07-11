
import sequtils

import syntax/il
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