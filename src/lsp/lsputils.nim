
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
    self in r and self != r.b
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
        self.ident.find(pos)
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
proc find*(self: GenTypeDef, pos: rPosition): Option[Ident] =
    if pos in self.id:
        some self.id
    else:
        if self.ub.isSome:
            self.ub.get.find(pos)
        else:
            none(Ident)
proc find*(self: Suite | Program, pos: rPosition): Option[Ident]
proc find*(self: Metadata, pos: rPosition): Option[Ident] =
    for e in self.params:
        result = e.find(pos)
        if result.isSome:
            return
proc find*(self: FunctionParam, pos: rPosition): Option[Ident] =
    for e in self.implicit:
        result = e.find(pos)
        if result.isSome:
            return
    for e in self.params:
        result = e.find(pos)
        if result.isSome:
            return
    if self.rety.isSome:
        result = self.rety.get.find(pos)
proc find*(self: Function, pos: rPosition): Option[Ident] =
    if pos in self.id:
        return some self.id
    result = self.param.find(pos)
    if result.isNone and self.suite.isSome:
        result = self.suite.get.find(pos)
    if result.isNone and self.metadata.isSome:
        result = self.metadata.get.find(pos)
proc find*(self: IdentDefSection, pos: rPosition): Option[Ident] =
    for iddef in self.iddefs:
        let res = iddef.find(pos)
        if res.isSome:
            return res
    none(Ident)
proc find*(self: Statement, pos: rPosition): Option[Ident] =
    case self.kind
    of StatementKind.Import:
        if pos in self.module:
            some(self.module)
        else:
            none(Ident)
    of StatementKind.For:
        none(Ident)
    of StatementKind.While:
        none(Ident)
    of StatementKind.Loop:
        none(Ident)
    of StatementKind.LetSection, StatementKind.VarSection:
        self.iddefSection.find(pos)
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
    of StatementKind.IndexAssign:
        if pos in self.id:
            self.id.find(pos)
        elif pos in self.index:
            self.index.find(pos)
        else:
            self.i_val.find(pos)
    of StatementKind.Funcdef:
        self.fn.find(pos)
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
        if self.label.isSome:
            if pos in self.label.get:
                return some self.label.get
        self.`block`.find(pos)
    of ExpressionKind.Lambda:
        none(Ident)
    of ExpressionKind.ObjCons:
        none(Ident)
    of ExpressionKind.Malloc:
        none(Ident)
    of ExpressionKind.Typeof:
        none(Ident)
    of ExpressionKind.Realloc:
        none(Ident)
    of ExpressionKind.Ptrset:
        none(Ident)
    of ExpressionKind.Ptrget:
        none(Ident)
    of ExpressionKind.Ref:
        none(Ident)
    of ExpressionKind.FnType:
        none(Ident)
    of ExpressionKind.IntCast:
        self.int_exp.find(pos)
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
    of il.SymbolKind.Field:
        lspschema.SymbolKind.Field
    of il.SymbolKind.Enum:
        lspschema.SymbolKind.Enum

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
    of il.SymbolKind.Field:
        CompletionItemKind.Field
    of il.SymbolKind.Enum:
        CompletionItemKind.Enum

converter toSemanticTokens*(self: il.SymbolKind): lspschema.SemanticTokenTypes =
    case self
    of il.SymbolKind.Var, il.SymbolKind.Let:
        SemanticTokenTypes.variable
    of il.SymbolKind.Param:
        SemanticTokenTypes.parameter
    of il.SymbolKind.Const:
        SemanticTokenTypes.variable
    of il.SymbolKind.Typ:
        SemanticTokenTypes.type
    of il.SymbolKind.GenParam:
        SemanticTokenTypes.typeParameter
    of il.SymbolKind.Func:
        SemanticTokenTypes.function
    of il.SymbolKind.Field:
        SemanticTokenTypes.parameter
    of il.SymbolKind.Enum:
        SemanticTokenTypes.enumMember


proc scope(self: Statement, pos: rPosition): seq[Scope]
proc scope(self: Suite, pos: rPosition): seq[Scope] =
    result = @[self.scope]
    for s in self.stmts:
        if pos in s:
            result.add s.scope(pos)

proc scope(self: Statement, pos: rPosition): seq[Scope] =
    case self.kind
    of StatementKind.Import:
        @[]
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
    of StatementKind.IndexAssign:
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
proc scope*(self: Program, pos: rPosition): Scope =
    var res = @[self.scope]
    for s in self.stmts:
        if pos in s:
            res.add s.scope(pos)
    res[^1]
