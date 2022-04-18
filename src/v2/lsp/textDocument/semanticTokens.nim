
import streams
import json
import tables
import strformat
import sequtils
import options

import ../lspschema
import ../lspprotocol
import ../configuration
import ../lsputils
import ../window

import ../../projects
import ../../il
from ../../lineinfos import nil


type
    rLocation = lineinfos.Location
    Color = (int, int, int, int, int)


proc index(s: seq[string], str: string): int =
    for (i, e) in s.pairs:
        if e == str:
            return i
    return -1
# proc coloring(self: , data: var seq[(int, int, int, int, int)])
# proc coloring(self: Pattern, data: var seq[(int, int, int, int, int)]) =
#     case self.kind
#     of PatternKind.Literal:
#         discard
#     of PatternKind.Ident:
#         self.id.coloring(data)
#     # of PatternKind.Range:
#     #     discard
#     # of PatternKind.Array:
#     #     discard
#     of PatternKind.Pair:
#         self.first.coloring(data)
#         self.second.coloring(data)
#     # of PatternKind.Tuple:
#     #     discard
#     of PatternKind.Record:
#         discard
#     of PatternKind.Discard:
#         discard
# proc coloring(self: FunctionParam, data: var seq[(int, int, int, int, int)]) =
#     let
#         gen = self.gen
#         params = self.params
#         rety = self.rety
#     for e in gen & params:
#         let
#             pat = e.pat
#             typ = e.typ
#             default = e.default
#         pat.coloring(data)
#         if typ.isSome:
#             typ.get.coloring(data)
#         if default.isSome:
#             default.get.coloring(data)
#     rety.coloring(data)
# proc coloring(self: IdentDef, data: var seq[(int, int, int, int, int)]) =
#     let
#         iddef = self
#         pat = iddef.pat
#         typ = iddef.typ
#         default = iddef.default
#     pat.coloring(data)
#     if typ.isSome:
#         typ.get.coloring(data)
#     if default.isSome:
#         default.get.coloring(data)
# proc coloring(self: Term, data: var seq[(int, int, int, int, int)]) =
#     if self.inserted:
#         return
#     case self.kind
#     of TermKind.Failed:
#         discard
#     of TermKind.bottom:
#         discard
#     of TermKind.`()`:
#         discard
#     of TermKind.Unit:
#         discard
#     of TermKind.U:
#         discard
#     # of TermKind.Bool:
#     #     discard
#     of TermKind.Integer, TermKind.Float:
#         let
#             kind = SemanticTokenTypes.number
#             loc = self.loc
#             a = loc.`range`.a
#             b = loc.`range`.b
#             l = b.character - a.character
#         assert a.line == b.line
#         data.add (a.line, a.character, l, tokenTypes.index($kind), 0)
#     of TermKind.Char:
#         discard
#     of TermKind.String:
#         discard
#     of TermKind.Id:
#         if not self.typ.isNil and self.typ.symbol.isSome:
#             let
#                 symbol = self.typ.symbol.get
#                 loc = self.loc
#                 a = loc.`range`.a
#                 b = loc.`range`.b
#                 l = b.character - a.character
#                 kind = case symbol.kind
#                     of il.SymbolKind.Var:
#                         SemanticTokenTypes.variable
#                     of il.SymbolKind.Let:
#                         SemanticTokenTypes.variable
#                     of il.SymbolKind.Const:
#                         SemanticTokenTypes.variable
#                     of il.SymbolKind.Typ:
#                         SemanticTokenTypes.type
#                     of il.SymbolKind.Func:
#                         SemanticTokenTypes.function
#             assert a.line == b.line, $a.line
#             data.add (a.line, a.character, l, tokenTypes.index($kind), 0)
#         else:
#             s.window.logMessage(fmt"{self} at {self.loc}")
#     # of TermKind.Lambda:
#     #     discard
#     # of TermKind.List:
#     #     discard
#     of TermKind.Tuple:
#         for e in self.terms:
#             e.coloring(data)
#     of TermKind.Record:
#         discard  # named tuple
#     of TermKind.Let, TermKind.Const:
#         for e in self.iddefs:
#             e.coloring(data)
#     # of TermKind.Typedef:
#     #     discard
#     of TermKind.Funcdef:
#         let
#             fn = self.fn
#             id = fn.id
#             param = fn.param
#             metadata = fn.metadata
#             body = fn.body
#         id.coloring(data)
#         param.coloring(data)
#         # TODO: metadata
#         body.term.coloring(data)
#     of TermKind.FuncdefInst:
#         discard
#     of TermKind.FunctionInst:
#         self.pfn.coloring(data)
#         for e in self.instargs:
#             e.coloring(data)
#     # of TermKind.If:
#     #     discard
#     # of TermKind.When:
#     #     discard
#     of TermKind.Case:
#         discard
#     # of TermKind.While:
#     #     discard
#     # of TermKind.For:
#     #     discard
#     # of TermKind.Loop:
#     #     discard
#     # of TermKind.Block:
#     #     discard
#     # of TermKind.Asign:
#     #     discard
#     of TermKind.Typeof, TermKind.Discard:
#         self.term.coloring(data)
#     of TermKind.Apply:
#         self.callee.coloring(data)
#         for e in self.args:
#             e.coloring(data)
#     of TermKind.Meta:
#         discard
#     of TermKind.Seq:
#         # self.terms.applyIt(it.coloring(data))
#         for e in self.terms:
#             e.coloring(data)
#     of TermKind.Us:
#         discard
proc coloring(self: Literal, tokenTypes: seq[string], loc: rLocation, data: var seq[Color]) =
    case self.kind
    of LiteralKind.unit:
        discard
    of LiteralKind.bool:
        discard
    of LiteralKind.integer, LiteralKind.float:
        let
            kind = SemanticTokenTypes.number
            a = loc.`range`.a
            b = loc.`range`.b
            l = b.character - a.character
        assert a.line == b.line
        data.add (a.line, a.character, l, tokenTypes.index($kind), 0)
    of LiteralKind.char:
        discard
    of LiteralKind.string:
        let
            kind = SemanticTokenTypes.string
            a = loc.`range`.a
            b = loc.`range`.b
            l = b.character - a.character
        assert a.line == b.line
        data.add (a.line, a.character, l, tokenTypes.index($kind), 0)
    of LiteralKind.Univ:
        discard
proc coloring(self: Ident, tokenTypes: seq[string], data: var seq[Color]) =
    if not self.typ.isNil and self.typ.symbol.isSome:
        let
            symbol = self.typ.symbol.get
            loc = self.loc
            a = loc.`range`.a
            b = loc.`range`.b
            l = b.character - a.character
            kind: SemanticTokenTypes = symbol.kind
        assert a.line == b.line, $a.line
        data.add (a.line, a.character, l, tokenTypes.index($kind), 0)
proc coloring(self: Statement, tokenTypes: seq[string], data: var seq[Color])
proc coloring(self: Suite, tokenTypes: seq[string], data: var seq[Color]) =
    for s in self.stmts:
        s.coloring(tokenTypes, data)
proc coloring(self: Expression, tokenTypes: seq[string], data: var seq[Color]) =
    case self.kind
    of ExpressionKind.Literal:
        self.litval.coloring(tokenTypes, self.loc, data)
    of ExpressionKind.Ident:
        self.ident.coloring(tokenTypes, data)
    of ExpressionKind.Tuple:
        discard
    of ExpressionKind.Array:
        discard
    of ExpressionKind.Record:
        discard
    of ExpressionKind.If:
        discard
    of ExpressionKind.When:
        discard
    of ExpressionKind.Case:
        discard
    of ExpressionKind.Call, ExpressionKind.Command:
        self.callee.coloring(tokenTypes, data)
        for e in self.args:
            e.coloring(tokenTypes, data)
    of ExpressionKind.Dot:
        discard
    of ExpressionKind.Bracket:
        discard
    of ExpressionKind.Binary:
        discard
    of ExpressionKind.Prefix:
        discard
    of ExpressionKind.Postfix:
        discard
    of ExpressionKind.Block:
        discard
    of ExpressionKind.Lambda:
        discard
    of ExpressionKind.Malloc:
        discard
    of ExpressionKind.Typeof:
        discard
    of ExpressionKind.Ref:
        discard
    of ExpressionKind.FnType:
        discard
    of ExpressionKind.Fail:
        discard

proc coloring(self: Pattern, tokenTypes: seq[string], data: var seq[Color]) =
    case self.kind
    of PatternKind.Literal:
        self.litval.coloring(tokenTypes, self.loc, data)
    of PatternKind.Ident:
        self.ident.coloring(tokenTypes, data)
    of PatternKind.Dot:
        discard
    of PatternKind.Tuple:
        for e in self.patterns:
            e.coloring(tokenTypes, data)
    of PatternKind.Record:
        discard
    of PatternKind.UnderScore:
        discard

proc coloring(self: GenTypeDef, tokenTypes: seq[string], data: var seq[Color]) =
    self.id.coloring(tokenTypes, data)
    if self.ub.isSome:
        self.ub.get.coloring(tokenTypes, data)
proc coloring(self: IdentDef, tokenTypes: seq[string], data: var seq[Color]) =
    self.pat.coloring(tokenTypes, data)
    if self.typ.isSome:
        self.typ.get.coloring(tokenTypes, data)
    if self.default.isSome:
        self.default.get.coloring(tokenTypes, data)
proc coloring(self: FunctionParam, tokenTypes: seq[string], data: var seq[Color]) =
    for e in self.implicit:
        e.coloring(tokenTypes, data)
    for e in self.params:
        e.coloring(tokenTypes, data)
    if self.rety.isSome:
        self.rety.get.coloring(tokenTypes, data)
proc coloring(self: Function, tokenTypes: seq[string], data: var seq[Color]) =
    self.param.coloring(tokenTypes, data)
    if self.suite.isSome:
        self.suite.get.coloring(tokenTypes, data)
proc coloring(self: Statement, tokenTypes: seq[string], data: var seq[Color]) =
    # TODO:
    case self.kind
    of StatementKind.For:
        discard
    of StatementKind.While:
        discard
    of StatementKind.Loop:
        discard
    of StatementKind.LetSection, StatementKind.VarSection:
        for e in self.iddefs:
            e.coloring(tokenTypes, data)
    of StatementKind.ConstSection:
        discard
    of StatementKind.TypeSection:
        discard
    of StatementKind.Asign:
        self.pat.coloring(tokenTypes, data)
        self.val.coloring(tokenTypes, data)
    of StatementKind.Funcdef:
        self.fn.coloring(tokenTypes, data)
    of StatementKind.Meta:
        discard
    of StatementKind.Discard:
        if self.`discard`.isSome:
            self.`discard`.get.coloring(tokenTypes, data)
    of StatementKind.Comments:
        let
            kind = SemanticTokenTypes.comment
            index = tokenTypes.index($kind)
            loc = self.loc
            line = loc.`range`.a.line
            character = loc.`range`.a.character
        for (i, c) in self.comments.pairs:
            data.add (line + i, if i == 0: character else: 0, c.len+1, index, 0)
    of StatementKind.Expression:
        self.expression.coloring(tokenTypes, data)
    of StatementKind.Fail:
        discard
proc coloring(self: Program, tokenTypes: seq[string], data: var seq[Color]) =
    for s in self.stmts:
        s.coloring(tokenTypes, data)
proc `textDocument/semanticTokens/full`*(s: Stream, msg: RequestMessage, configuration: Configuration, project: Project) =
    let
        tokenTypes = configuration.tokenTypes
        tokenModifiers = configuration.tokenModifiers
    if msg.params.isValid(SemanticTokensParams):
        let
            params = SemanticTokensParams(msg.params)
            textDocument = params["textDocument"]
            uri = textDocument["uri"].getStr
        s.window.logMessage("[textDocument/semanticTokens/full] " & uri)
        var data: seq[int] = @[]
        if uri in project.program:
            let
                program = project.program[uri]
            var
                data2: seq[Color] = @[]
                prevLine = 0
                prevCh = 0

            program.coloring(tokenTypes, data2)
            for (line, ch, l, typ, mods) in data2:
                if typ notin 0..<tokenTypes.len:
                    continue
                data.add if prevLine == line:
                    assert ch > prevCh, fmt"{ch} > {prevCh}, line: {line}"
                    @[0, ch - prevCh, l, typ, mods]
                else:
                    assert line > prevLine, fmt"{line} > {prevLine}"
                    @[line - prevLine, ch, l, typ, mods]
                prevLine = line
                prevCh = ch
            s.window.logMessage($data)
        s.respond(msg):
            if data.len == 0:
                newJNull()
            else:
                SemanticTokens.create(
                    none(string),
                    data
                ).JsonNode
        s.window.logMessage("[textDocument/semanticTokens/full]: responded")
    else:
        s.window.logMessage("[textDocument/semanticTokens/full]: invalid params")
