
import sequtils
import tables
import sugar

import syntax/il
import syntax/projects as ilprojects
import sema/projects as irprojects
import sema/ir/[
    ir, 
    constructors
]

import lineinfos

proc il2ir*(self: il.IdentDef, scope: Scope): ir.IdentDef =
    # TODO:
    discard

proc il2ir*(self: il.TypeDef, scope: Scope): ir.TypeDef =
    # TODO:
    discard

proc il2ir*(self: IdentDefSection, scope: Scope): seq[ir.IdentDef] =
    self.iddefs.map(_ => il2ir(_, scope))

proc il2ir*(self: TypeDefSection, scope: Scope): seq[ir.TypeDef] =
    self.typedefs.map(_ => il2ir(_, scope))

proc il2ir*(self: Statement, scope: Scope): ir.Expression =
    # TODO:
    # to be deleted
    let 
        unitLit = ir.Literal.Unit
        unit = ir.Expression.Lit(unitLit, self.loc)
    case self.kind:
    of StatementKind.LetSection:
        ir.Expression.LetSection(self.iddefSection.il2ir(scope), self.loc)
    of StatementKind.VarSection:
        ir.Expression.VarSection(self.iddefSection.il2ir(scope), self.loc)
    of StatementKind.ConstSection:
        ir.Expression.ConsSection(self.iddefSection.il2ir(scope), self.loc)
    of StatementKind.TypeSection:
        ir.Expression.TypeSection(self.typedefSection.il2ir(scope), self.loc)
    of StatementKind.Funcdef:
        unit
    of StatementKind.Expression:
        unit
    else:
        unit

proc il2ir*(self: Program): ir.Expression =
    let scope = newScope()
    ir.Expression.Seq(
        self.stmts.map(_ => il2ir(_, scope)), 
        scope,
        # TODO:
        # calculate location from stmts or 
        # change definition of Program to have location and revise parser of Program
        newLocation()
    )

proc il2ir*(self: ilprojects.Project): irprojects.Project =
    result = irprojects.newProject(self.main)
    for uri, program in self.program.pairs:
        result.programs[uri] = program.il2ir