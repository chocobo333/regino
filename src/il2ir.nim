
import sequtils
import tables
import options
import sugar
import algorithm

from syntax/il import loc
import syntax/projects as ilprojects
import sema/projects as irprojects
import sema/ir
import sema/typeenvs

import lineinfos

let
    unitExpression = ir.Expression.Lit(ir.Literal.Unit, newLocation())
    unitTypeExpression = ir.TypeExpression.Expr(unitExpression)

proc unit(scope: Scope): ir.Expression =
    result = ir.Expression.Lit(ir.Literal.Unit, newLocation())
    result.scope = scope

proc il2ir*(self: il.Suite, scope: Scope): ir.Expression

proc il2ir*(self: il.Ident, scope: Scope): ir.Ident =
    constructors.newIdent(self.name, self.loc)

proc il2ir*(self: il.Literal, scope: Scope): ir.Literal =
    case self.kind:
    of il.LiteralKind.unit:
        ir.Literal.Unit
    of il.LiteralKind.bool:
        ir.Literal.Bool(self.boolval)
    of il.LiteralKind.integer:
        ir.Literal.Integer(self.intval, self.intbits)
    of il.LiteralKind.float:
        ir.Literal.Float(self.floatval, self.floatbits)
    of il.LiteralKind.char:
        ir.Literal.Char(self.charval)
    of il.LiteralKind.string:
        ir.Literal.CString(self.strval)
    of il.LiteralKind.Univ:
        ir.Literal.Univ(self.level)

proc il2ir*(self: il.Expression, scope: Scope): ir.Expression =
    # TODO:
    result = case self.kind:
    of il.ExpressionKind.Literal:
        ir.Expression.Lit(self.litval.il2ir(scope), self.loc)
    of il.ExpressionKind.Ident:
        ir.Expression.Id(self.ident.il2ir(scope), self.loc)
    of il.ExpressionKind.Tuple:
        assert false, "notimplemented"
        unitExpression
    of il.ExpressionKind.Array:
        ir.Expression.Array(self.exprs.map(it => it.il2ir(scope)), self.loc)
    of il.ExpressionKind.Record:
        assert false, "notimplemented"
        unitExpression
    of il.ExpressionKind.ObjCons:
        assert false, "notimplemented"
        unitExpression
    of il.ExpressionKind.If:
        proc makeIf(elifs: seq[il.ElifBranch], scope: Scope): ir.Expression =
            if elifs.len == 0:
                if self.elseb.isSome:
                    self.elseb.get.il2ir(scope)
                else:
                    let
                        nScope = newScope(scope)
                        elseb = ir.Expression.Seq(@[unit(nScope)], self.loc)
                    elseb.scope = nScope
                    elseb
            else:
                let
                    head = elifs[0]
                    tail = elifs[1..^1]

                let
                    nScope = newScope(scope)
                    els = ir.Expression.Seq(@[makeIf(tail, nScope)], self.loc)
                els.scope = nScope

                let ret = ir.Expression.If(head.cond.il2ir(scope), head.suite.il2ir(scope), els, self.loc)
                ret.scope = scope
                ret

        makeIf(self.elifs, scope)
    of il.ExpressionKind.When:
        assert false, "notimplemented"
        unitExpression
    of il.ExpressionKind.Case:
        assert false, "notimplemented"
        unitExpression
    of il.ExpressionKind.Call, il.ExpressionKind.Command:
        ir.Expression.Call(self.callee.il2ir(scope), self.args.map(it => it.il2ir(scope)), self.loc)
    of il.ExpressionKind.Bracket:
        ir.Expression.Apply(self.callee.il2ir(scope), self.args.map(it => it.il2ir(scope)), self.loc)
    of il.ExpressionKind.Dot:
        ir.Expression.Call(self.rhs.il2ir(scope), self.lhs.il2ir(scope) & self.dotArgs.map(it => it.il2ir(scope)), self.loc)
    of il.ExpressionKind.Binary:
        let callee = ir.Expression.Id(self.op.il2ir(scope), self.loc)
        ir.Expression.Call(callee, @[self.lhs.il2ir(scope), self.rhs.il2ir(scope)], self.loc)
    of il.ExpressionKind.Prefix:
        assert false, "notimplemented"
        unitExpression
    of il.ExpressionKind.Postfix:
        assert false, "notimplemented"
        unitExpression
    of il.ExpressionKind.Block:
        assert false, "notimplemented"
        unitExpression
    of il.ExpressionKind.Lambda:
        assert false, "notimplemented"
        unitExpression
    of il.ExpressionKind.Malloc:
        ir.Expression.Malloc(self.mtype.il2ir(scope), self.msize.il2ir(scope), self.loc)
    of il.ExpressionKind.Realloc:
        ir.Expression.Realloc(self.rptr.il2ir(scope), self.msize.il2ir(scope), self.loc)
    of il.ExpressionKind.Typeof:
        ir.Expression.Typeof(self.`typeof`.il2ir(scope), self.loc)
    of il.ExpressionKind.Ptrset:
        ir.Expression.PtrSet(self.`ptr`.il2ir(scope), self.idx.il2ir(scope), self.v.il2ir(scope), self.loc)
    of il.ExpressionKind.Ptrget:
        ir.Expression.PtrGet(self.`ptr`.il2ir(scope), self.idx.il2ir(scope), self.loc)
    of il.ExpressionKind.Ref:
        ir.Expression.Ref(self.`ref`.il2ir(scope), self.loc)
    of il.ExpressionKind.FnType:
        assert false, "notimplemented"
        unitExpression
    of il.ExpressionKind.IntCast:
        assert false, "notimplemented"
        unitExpression
    of il.ExpressionKind.Fail:
        assert false, "notimplemented"
        unitExpression

    result.scope = scope

proc il2ir*(self: il.TypeExpression, scope: Scope): ir.TypeExpression =
    # TODO:
    result = case self.kind:
    of il.TypeExpressionKind.Object:
        assert false, "notimplemented"
        unitTypeExpression
    of il.TypeExpressionKind.Sum:
        assert false, "notimplemented"
        unitTypeExpression
    of il.TypeExpressionKind.Distinct:
        assert false, "notimplemented"
        unitTypeExpression
    of il.TypeExpressionKind.Trait:
        assert false, "notimplemented"
        unitTypeExpression
    of il.TypeExpressionKind.Expression:
        ir.TypeExpression.Expr(self.expression.il2ir(scope))

    if self.isRef:
        result = ir.TypeExpression.Ref(result)


proc il2ir*(self: il.Pattern, scope: Scope): ir.Pattern =
    case self.kind:
    of il.PatternKind.Literal:
        ir.Pattern.Lit(self.litval.il2ir(scope))
    of il.PatternKind.Ident:
        ir.Pattern.Id(self.ident.il2ir(scope))
    of il.PatternKind.Tuple:
        ir.Pattern.Tuple(self.tag.map(it => it.il2ir(scope)), self.patterns.map(it => it.il2ir(scope)))
    of il.PatternKind.Record:
        ir.Pattern.Record(self.tag.map(it => it.il2ir(scope)), self.members.map(_ => (il2ir(_[0], scope), il2ir(_[1], scope))))
    of il.PatternKind.UnderScore:
        let ident = constructors.newIdent("_", self.loc)
        ir.Pattern.Id(ident)

proc il2ir*(self: il.IdentDef, scope: Scope): ir.IdentDef =
    ir.IdentDef(
        pat: self.pat.il2ir(scope),
        typ: self.typ.map(it => it.il2ir(scope)),
        default: self.default.map(it => it.il2ir(scope)),
        loc: self.loc
    )

proc il2ir*(self: il.GenTypeDef, scope: Scope): ir.GenTypeDef =
    ir.GenTypeDef(
        ident: self.id.il2ir(scope),
        typ: self.typ.map(it => it.il2ir(scope)),
        ub: self.ub.map(it => it.il2ir(scope)),
        loc: self.loc
    )

proc il2ir*(self: il.TypeDef, scope: Scope): ir.Expression =
    let
        scope = newScope(scope)
        params = if self.params.isSome: self.params.get.map(it => it.il2ir(scope)) else: @[]
        typeDef = ir.TypeDef(
            ident: self.id.il2ir(scope),
            params: params,
            typ: self.typ.il2ir(scope),
            loc: self.loc
        )
    result = ir.Expression.TypeSection(typeDef, self.loc)
    result.scope = scope

proc il2ir*(self: il.IdentDefSection, scope: Scope): seq[ir.IdentDef] =
    self.iddefs.map(it => it.il2ir(scope))

proc il2ir*(self: il.TypeDefSection, scope: Scope): seq[ir.Expression] =
    result = self.typedefs.map(it => it.il2ir(scope))

proc f2fs*(self: il.Function, scope: Scope): FunctionSignature =
    FunctionSignature(
        ident: self.id.il2ir(scope),
        implicits: self.param.implicit.map(it => it.il2ir(scope)),
        params: self.param.params.map(it => it.il2ir(scope)),
        # TODO: when rety is none
        rety: if self.param.rety.isSome: self.param.rety.get.il2ir(scope) else: unitExpression
    )

proc il2ir*(self: il.Function, scope: Scope): Function =
    let
        signature = f2fs(self, scope)
        body = if self.suite.isSome: self.suite.get.il2ir(scope) else: unitExpression

    Function(
        signature: signature,
        body: body
    )

proc il2ir*(self: il.Statement, scope: Scope): ir.Expression =
    # TODO:
    var nScope = scope
    result = case self.kind:
    of il.StatementKind.Import:
        assert false, "notimplemented"
        unitExpression
    of il.StatementKind.For:
        assert false, "notimplemented"
        unitExpression
    of il.StatementKind.While:
        assert false, "notimplemented"
        unitExpression
    of il.StatementKind.Loop:
        assert false, "notimplemented"
        unitExpression
    of il.StatementKind.LetSection:
        ir.Expression.LetSection(self.iddefSection.il2ir(scope), self.loc)
    of il.StatementKind.VarSection:
        ir.Expression.VarSection(self.iddefSection.il2ir(scope), self.loc)
    of il.StatementKind.ConstSection:
        ir.Expression.ConstSection(self.iddefSection.il2ir(scope), self.loc)
    of il.StatementKind.TypeSection:
        let typeDefs = self.typedefSection.il2ir(scope)
        ir.Expression.Seq(typeDefs, self.loc)
    of il.StatementKind.Asign:
        assert false, "notimplemented"
        unitExpression
    of il.StatementKind.IndexAssign:
        assert false, "notimplemented"
        unitExpression
    of il.StatementKind.Funcdef:
        let f = self.fn
        if f.metadata.isSome and f.metadata.get.kind == il.MetadataKind.ImportLL:
            nScope = newScope(scope)
            ir.Expression.ImportLL(self.fn.f2fs(nScope), self.loc)
        else:
            ir.Expression.Funcdef(self.fn.il2ir(scope), self.loc)
    of il.StatementKind.Meta:
        # assert false, "notimplemented"
        unitExpression
    of il.StatementKind.Discard:
        assert false, "notimplemented"
        unitExpression
    of il.StatementKind.Comments:
        assert false, "il.Comment cannnot be translated to ir"
        unitExpression
    of il.StatementKind.Expression:
        self.expression.il2ir(scope)
    of il.StatementKind.Fail:
        assert false, "notimplemented"
        unitExpression

    result.scope = nScope

proc il2ir*(self: il.Suite, scope: Scope): ir.Expression =
    let scope = newScope(scope)
    result = ir.Expression.Seq(self.stmts.filterIt(it.kind != il.StatementKind.Comments).map(it => it.il2ir(scope)), self.loc)
    result.scope = scope

proc il2ir*(self: il.Program): ir.Expression =
    let scope = newScope()
    result = ir.Expression.Seq(
        self.stmts.filterIt(it.kind != il.StatementKind.Comments).map(it => it.il2ir(scope)),
        # TODO:
        # calculate location from stmts or
        # change definition of Program to have location and revise parser of Program
        newLocation()
    )
    result.scope = scope

proc il2ir*(self: ilprojects.Project): irprojects.Project =
    result = irprojects.newProject(self.main)
    for uri, program in self.program.pairs:
        result.programs[uri] = program.il2ir

when isMainModule:
    import os
    import syntax/projects
    import sema/sema
    import utils
    let
        mainPath = "test/test05.rgn".absolutePath
        ilProject = buildProject(mainPath)
        irProject = ilProject.il2ir
    irProject.sema
