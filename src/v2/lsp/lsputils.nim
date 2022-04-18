
import options
import sequtils
import strutils
import sugar
import options
import json


import ../il
import ../lineinfos except Position, Location

import lspschema

type
    rPosition = lineinfos.Position
proc to*(self: rPosition): Position =
    Position.create(
        self.line,
        self.character,
    )
proc to*(self: PosRange): Range =
    Range.create(
        self.a.to,
        self.b.to,
    )

proc to*(self: lineinfos.Location): Location =
    Location.create(
        self.uri.path,
        self.range.to
    )

proc `in`*(self: rPosition, term: Statement|Expression|Ident|Pattern): bool =
    let r = term.loc.`range`
    self in r
proc `in`*(self: rPosition, term: Suite): bool =
    term.stmts.anyIt(self in it)

proc find*(self: Ident, pos: rPosition): Option[Ident] =
    if pos in self:
        some self
    else:
        none(Ident)
proc find*(self: Expression, pos: rPosition): Option[Ident]
proc find*(self: Pattern, pos: rPosition): Option[Ident] =
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
        for e in self.patterns:
            if pos in e:
                return e.find(pos)
        none(Ident)
    of PatternKind.Record:
        none(Ident)
    of PatternKind.UnderScore:
        none(Ident)
proc find*(self: IdentDef, pos: rPosition): Option[Ident] =
    if self.typ.isSome and pos in self.typ.get:
        self.typ.get.find(pos)
    elif self.default.isSome and pos in self.default.get:
        self.default.get.find(pos)
    else:
        self.pat.find(pos)
proc find*(self: Statement, pos: rPosition): Option[Ident] =
    case self.kind
    of StatementKind.For:
        none(Ident)
    of StatementKind.While:
        none(Ident)
    of StatementKind.Loop:
        none(Ident)
    of StatementKind.LetSection, StatementKind.VarSection:
        for iddef in self.iddefs:
            let res = iddef.find(pos)
            if res.isSome:
                return res
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

proc find*(self: Suite | Program, pos: rPosition): Option[Ident] =
    for e in self.stmts:
        if pos in e:
            return e.find(pos)
proc find*(self: Expression, pos: rPosition): Option[Ident] =
    case self.kind
    of ExpressionKind.Literal:
        none(Ident)
    of ExpressionKind.Ident:
        self.ident.find(pos)
    of ExpressionKind.Tuple, ExpressionKind.Array:
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

converter toSymbolKind*(self: il.SymbolKind): lspschema.SymbolKind =
    case self:
    of il.SymbolKind.Var, il.SymbolKind.Let, il.SymbolKind.Param:
        lspschema.SymbolKind.Variable
    of il.SymbolKind.Const:
        lspschema.SymbolKind.Constant
    of il.SymbolKind.Typ, il.SymbolKind.GenParam:
        lspschema.SymbolKind.Class
    of il.SymbolKind.Func:
        lspschema.SymbolKind.Function

converter toCompletionItemKind*(self: il.SymbolKind): lspschema.CompletionItemKind =
    case self:
    of il.SymbolKind.Var, il.SymbolKind.Let, il.SymbolKind.Param:
        CompletionItemKind.Variable
    of il.SymbolKind.Const:
        CompletionItemKind.Constant
    of il.SymbolKind.Typ:
        CompletionItemKind.Class
    of il.SymbolKind.GenParam:
        CompletionItemKind.TypeParameter
    of il.SymbolKind.Func:
        CompletionItemKind.Function


proc scope(self: Statement, pos: rPosition): seq[Scope]
proc scope(self: Suite, pos: rPosition): seq[Scope] =
    result = @[self.scope]
    for s in self.stmts:
        if pos in s:
            result.add s.scope(pos)

proc scope(self: Statement, pos: rPosition): seq[Scope] =
    case self.kind
    of StatementKind.While:
        @[]
    of StatementKind.For:
        @[]
    of StatementKind.Loop:
        @[]
    of StatementKind.LetSection:
        @[]
    of StatementKind.VarSection:
        @[]
    of StatementKind.ConstSection:
        @[]
    of StatementKind.TypeSection:
        @[]
    of StatementKind.Asign:
        @[]
    of StatementKind.Funcdef:
        # TODO: for param
        if self.fn.suite.isSome and pos in self.fn.suite.get:
            @[self.fn.suite.get.scope] & scope(self.fn.suite.get, pos)
        else:
            @[]
    of StatementKind.Meta:
        @[]
    of StatementKind.Discard:
        @[]
    of StatementKind.Comments:
        @[]
    of StatementKind.Expression:
        @[]
    of StatementKind.Fail:
        @[]
proc scope*(self: Program, pos: rPosition): seq[Scope] =
    result = @[self.scope]
    for s in self.stmts:
        if pos in s:
            result.add s.scope(pos)
