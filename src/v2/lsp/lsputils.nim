
import options
import sequtils
import sugar

import ../il
import ../lineinfos

from lspschema import nil


proc `in`*(self: Position, term: Statement|Expression|Ident): bool =
    let r = term.loc.`range`
    self != r.b and self in r
proc `in`*(self: Position, term: Suite): bool =
    term.stmts.anyIt(self in it)

proc find*(self: Ident, pos: Position): Option[Ident] =
    if pos in self:
        some self
    else:
        none(Ident)
proc find*(self: Expression, pos: Position): Option[Ident]
proc find*(self: Pattern, pos: Position): Option[Ident] =
    case self.kind
    of PatternKind.Literal:
        none(Ident)
    of PatternKind.Ident:
        if self.index.isSome and pos in self.index.get:
            self.index.get.find(pos)
        else:
            self.ident.find(pos)
    of PatternKind.Dot:
        none(Ident)
    of PatternKind.Tuple:
        none(Ident)
    of PatternKind.Record:
        none(Ident)
    of PatternKind.UnderScore:
        none(Ident)
proc find*(self: IdentDef, pos: Position): Option[Ident] =
    if self.typ.isSome and pos in self.typ.get:
        self.typ.get.find(pos)
    elif self.default.isSome and pos in self.default.get:
        self.default.get.find(pos)
    else:
        self.pat.find(pos)
proc find*(self: Statement, pos: Position): Option[Ident] =
    case self.kind
    of StatementKind.For:
        none(Ident)
    of StatementKind.While:
        none(Ident)
    of StatementKind.Loop:
        none(Ident)
    of StatementKind.LetSection:
        for iddef in self.iddefs:
            let res = iddef.find(pos)
            if res.isSome:
                return res
        none(Ident)
    of StatementKind.VarSection:
        none(Ident)
    of StatementKind.ConstSection:
        none(Ident)
    of StatementKind.TypeSection:
        none(Ident)
    of StatementKind.Asign:
        if pos in self.val:
            self.val.find(pos)
        elif pos in self.op:
            self.op.find(pos)
        else:
            self.pat.find(pos)
    of StatementKind.Funcdef:
        none(Ident)
    of StatementKind.Meta:
        none(Ident)
    of StatementKind.Discard:
        self.`discard`.map(it => it.find(pos)).get(none(Ident))
    of StatementKind.Comments:
        none(Ident)
    of StatementKind.Expression:
        self.expression.find(pos)
    of StatementKind.Fail:
        none(Ident)

proc find*(self: Suite | Program, pos: Position): Option[Ident] =
    for e in self.stmts:
        if pos in e:
            return e.find(pos)
proc find*(self: Expression, pos: Position): Option[Ident] =
    case self.kind
    of ExpressionKind.Literal:
        none(Ident)
    of ExpressionKind.Ident:
        self.ident.find(pos)
    of ExpressionKind.Tuple, ExpressionKind.Seq:
        for e in self.exprs:
            if pos in e:
                return e.find(pos)
        none(Ident)
    of ExpressionKind.Record:
        none(Ident)
    of ExpressionKind.If, ExpressionKind.When:
        for e in self.elifs:
            if pos in e.cond:
                return e.cond.find(pos)
            elif pos in e.suite:
                return e.suite.find(pos)
        if self.elseb.isSome:
            self.elseb.get.find(pos)
        else:
            none(Ident)
    of ExpressionKind.Case:
        none(Ident)
    of ExpressionKind.Call, ExpressionKind.Command:
        for e in self.args:
            if pos in e:
                return e.find(pos)
        self.callee.find(pos)
    of ExpressionKind.Dot, ExpressionKind.Binary:
        if pos in self.lhs:
            self.lhs.find(pos)
        elif pos in self.rhs:
            self.rhs.find(pos)
        else:
            self.op.find(pos)
    of ExpressionKind.Bracket:
        none(Ident)
    of ExpressionKind.Prefix, ExpressionKind.Postfix:
        if pos in self.op:
            self.op.find(pos)
        else:
            self.expression.find(pos)
    of ExpressionKind.Block:
        none(Ident)
    of ExpressionKind.Lambda:
        none(Ident)
    of ExpressionKind.Malloc:
        none(Ident)
    of ExpressionKind.Typeof:
        none(Ident)
    of ExpressionKind.Ref:
        none(Ident)
    of ExpressionKind.FnType:
        none(Ident)
    of ExpressionKind.Fail:
        none(Ident)

converter to*(self: il.SymbolKind): lspschema.SymbolKind =
    case self:
    of il.SymbolKind.Var, il.SymbolKind.Let, il.SymbolKind.Param:
        lspschema.SymbolKind.Variable
    of il.SymbolKind.Const:
        lspschema.SymbolKind.Constant
    of il.SymbolKind.Typ, il.SymbolKind.GenParam:
        lspschema.SymbolKind.Class
    of il.SymbolKind.Func:
        lspschema.SymbolKind.Function
