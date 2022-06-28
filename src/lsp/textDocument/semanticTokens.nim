
import streams
import json
import tables
import strformat
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
    if self.inserted:
        return
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
        for e in self.elifs:
            let
                cond = e.cond
                suite = e.suite
            cond.coloring(tokenTypes, data)
            suite.coloring(tokenTypes, data)
        if self.elseb.isSome:
            self.elseb.get.coloring(tokenTypes, data)
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
    of ExpressionKind.ObjCons:
        discard
    of ExpressionKind.Malloc:
        discard
    of ExpressionKind.Typeof:
        discard
    of ExpressionKind.Realloc:
        discard
    of ExpressionKind.Ptrset:
        discard
    of ExpressionKind.Ptrget:
        discard
    of ExpressionKind.Ref:
        discard
    of ExpressionKind.FnType:
        discard
    of ExpressionKind.IntCast:
        discard
    of ExpressionKind.Fail:
        discard

proc coloring(self: Pattern, tokenTypes: seq[string], data: var seq[Color]) =
    case self.kind
    of PatternKind.Literal:
        self.litval.coloring(tokenTypes, self.loc, data)
    of PatternKind.Ident:
        self.ident.coloring(tokenTypes, data)
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
proc coloring(self: IdentDefSection, tokenTypes: seq[string], data: var seq[Color]) =
    for e in self.iddefs:
        e.coloring(tokenTypes, data)
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
        self.iddefSection.coloring(tokenTypes, data)
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
            character = loc.`range`.a.character
        var i = 0
        for l in (loc.`range`.a.line..loc.`range`.b.line):
            data.add (l, if i == 0: character else: 0, 0xff, index, 0)
            inc i
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
                    assert line > prevLine, fmt"{line} > {prevLine}, {tokenTypes[typ]}"
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
