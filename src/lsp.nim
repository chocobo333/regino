
import sequtils
import strutils
import strformat
import streams
import json
import tables
import options

import lsp/[
    lspschema,
    lspprotocol,
    configuration
]
import
    il,
    parsers,
    sema,
    codegen,
    utils
import lineinfos except Position
type rPosition = lineinfos.Position
import buffers


proc id(self: RequestMessage): int =
    let id = self["id"]
    case id.kind
    of JString:
        id.getStr.parseInt
    of JInt:
        id.getInt
    else:
        raise newException(IllformedError, "")
proc `method`(self: RequestMessage or NotificationMessage): string =
    self["method"].getStr
proc `params`(self: RequestMessage or NotificationMessage): JsonNode =
    let
        params = self["params"]
    if params.isSome:
        params.get
    else:
        newJNull()

proc response(self: RequestMessage, n: JsonNode): JsonNode =
    ResponseMessage.create(jsonrpc, self.id, some n, none ResponseError).JsonNode
proc respond(s: Stream, msg: RequestMessage, n: JsonNode) =
    s.sendMessage msg.response(n)
proc notify(s: Stream, `method`: string) =
    s.sendMessage NotificationMessage.create(jsonrpc, `method`, none JsonNode).JsonNode
proc notify(s: Stream, `method`: string, params: JsonNode) =
    s.sendMessage NotificationMessage.create(jsonrpc, `method`, some params).JsonNode
proc notify(s: Stream, p: (string, JsonNode)) =
    s.notify(p[0], p[1])
type
    Window = object
        s: Stream
    TextDocument = object
        s: Stream
        uri: string
proc window(s: Stream): Window = Window(s: s)
proc textDocument(s: Stream, uri: string): TextDocument = TextDocument(s: s, uri: uri)
proc `window/showMessage`(msg: string, msgtype: MessageType = MessageType.Log): (string, JsonNode) =
    (
        "window/showMessage",
        ShowMessageParams.create(
            msgtype.int,
            msg
        ).JsonNode
    )
proc `window/logMessage`(msg: string, msgtype: MessageType = MessageType.Log): (string, JsonNode) =
    (
        "window/logMessage",
        ShowMessageParams.create(
            msgtype.int,
            msg
        ).JsonNode
    )
proc showMessage(window: Window, msg: string, msgtype: MessageType = MessageType.Log) =
    window.s.notify `window/showMessage`("[regino]: " & msg, msgtype)
proc logMessage(window: Window, msg: string, msgtype: MessageType = MessageType.Log) =
    window.s.notify `window/logMessage`("[regino]: " & msg, msgtype)

proc `textDocument/publishDiagnostics`(uri: string, diags: seq[Diagnostic]): (string, JsonNode) =
    (
        "textDocument/publishDiagnostics",
        PublishDiagnosticsParams.create(
            uri,
            none int,
            diags
        ).JsonNode
    )
proc publishDiagnostics(textDocument: TextDocument, diags: seq[Diagnostic]) =
    textDocument.s.notify `textDocument/publishDiagnostics`(textDocument.uri, diags)

proc initilize(s: Stream, msg: RequestMessage, configuration: var Configuration, buffers: var Buffers) =
    if msg.params.isValid(InitializeParams, allowExtra=true):
        let
            params = InitializeParams(msg.params)
            capabilities = params["capabilities"]
            textDocument = capabilities["textDocument"]
            documentSymbol = textDocument["documentSymbol"].DocumentSymbolClientCapabilities
            semanticTokens = textDocument["semanticTokens"]
            tokenTypes = semanticTokens["tokenTypes"].to(seq[string])
            tokenModifiers = semanticTokens["tokenModifiers"].to(seq[string])
        configuration.tokenTypes = tokenTypes
        configuration.tokenModifiers = tokenModifiers
        configuration.documentSymbol = documentSymbol
        s.window.logMessage("Got initilize request")
        s.respond(msg):
            InitializeResult.create(
                ServerCapabilities.create(
                    some TextDocumentSyncOptions.create(
                        some true,
                        some TextDocumentSyncKind.Full.int
                    ),
                    some true, # Hover
                    some DocumentSymbolOptions.create(
                        none(bool),
                        some "regino"
                    ),
                    some SemanticTokensOptions.create(
                        none(bool),
                        SemanticTokensLegend.create(
                            tokenTypes,
                            tokenModifiers
                        ),
                        some false,
                        some true
                    ),
                ),
                some ServerInfo.create(
                    "regino-languages-server",
                    some "0.1.0"
                )
            ).JsonNode
proc `in`(self: rPosition, term: Term): bool =
    let r = term.loc.`range`
    self != r.b and self in r
# proc `in`(self: rPosition, pat: Pattern): bool =
#     let r = pat.loc.`range`
#     self != r.b and self in r
proc find(self: Term, pos: rPosition): Option[Term]
# proc find(self: Pattern, pos: rPosition): Option[Term] =
#     case self.kind
#     of PatternKind.Literal, PatternKind.Ident:
#         self.id.find(pos)
#     of PatternKind.Pair:
#         if pos in self.first:
#             self.first.find(pos)
#         else:
#             self.second.find(pos)
#     of PatternKind.Record:
#         none(Term)
#     of PatternKind.Discard:
#         none(Term)
proc find(self: IdentDef, pos: rPosition): Option[Term] =
    let
        iddef = self
        pat = iddef.pat
        typ = iddef.typ
        default = iddef.default
    var res = none(Term)
    block:
        if pos in pat:
            res = pat.find(pos)
            break
        if typ.isSome and pos in typ.get:
            res = typ.get.find(pos)
            break
        if default.isSome and pos in default.get:
            res = default.get.find(pos)
            break
    res
proc find(self: Term, pos: rPosition): Option[Term] =
    case self.kind
    of TermKind.Failed..TermKind.Id:
        if pos in self:
            some self
        else:
            none(Term)
    of TermKind.Tuple:
        var res = none(Term)
        for e in self.terms:
            if pos in e:
                res = e.find(pos)
                break
        res
    of TermKind.Record:
        none(Term)
    of TermKind.Let, TermKind.Const:
        var res = none(Term)
        for e in self.iddefs:
            res = e.find(pos)
            if res.isSome:
                break
        res
    of TermKind.Funcdef:
        none(Term)
    of TermKind.FuncdefInst:
        none(Term)
    of TermKind.FunctionInst:
        var res = none(Term)
        block:
            if pos in self.pfn:
                res = self.pfn.find(pos)
                break
            for e in self.instargs:
                if pos in e:
                    res = e.find(pos)
                    break
        res
    of TermKind.Case:
        none(Term)
    of TermKind.Typeof, TermKind.Discard:
        if pos in self.term:
            self.term.find(pos)
        else:
            none(Term)
    of TermKind.Apply:
        var res = none(Term)
        block:
            if pos in self.callee:
                res = self.callee.find(pos)
                break
            for e in self.args:
                if pos in e:
                    res = e.find(pos)
                    break
        res
    of TermKind.Meta:
        none(Term)
    of TermKind.Seq:
        var res = none(Term)
        for e in self.terms:
            if pos in e:
                res = e.find(pos)
                break
        res
    of TermKind.Us:
        if pos in self:
            some self
        else:
            none(Term)
proc `textDocument/hover`(s: Stream, msg: RequestMessage, configuration: Configuration, buffers: var Buffers) =
    if msg.params.isValid(HoverParams):
        let
            params = HoverParams(msg.params)
            textDocument = params["textDocument"]
            pos = params["position"].to(rPosition)
            uri = textDocument["uri"].getStr
        var data: seq[string]
        if uri in buffers.termbuf:
            let
                term = buffers.termbuf[uri] # main function
                focus = term.fn.body.term.find(pos)
            if focus.isSome:
                let focus = focus.get
                if focus.typ.symbol.isSome:
                    data.add $focus.typ.symbol.get
                if focus.kind == TermKind.Id:
                    data.add fmt"{focus.name}: {focus.typ}"
                else:
                    data.add $focus.typ
            s.window.logMessage("[textDocument/hover]: " & $focus)
        s.respond(msg):
            if data.len == 0:
                newJNull()
            else:
                Hover.create(
                    data,
                    none(Range)
                ).JsonNode
    else:
        s.window.logMessage("[textDocument/hover]: valid params")
proc collectSymbol(self: Term): Option[JsonNode] =
    # result type is DocumentSymbol[]
    case self.kind
    of TermKind.Failed..TermKind.Const:
        none(JsonNode)
    of TermKind.Funcdef:
        var res = newJArray()
        for it in toSeq(self.fn.body.scope.syms.values).flatten:
            let
                kind = case it.kind:
                of il.SymbolKind.Var, il.SymbolKind.Let:
                    lspschema.SymbolKind.Variable
                of il.SymbolKind.Const:
                    lspschema.SymbolKind.Constant
                of il.SymbolKind.Typ:
                    lspschema.SymbolKind.Class
                of il.SymbolKind.Func:
                    lspschema.SymbolKind.Function
            res.add DocumentSymbol.create(
                it.decl.name,
                some $it.typ,
                kind.int,
                none(seq[int]),
                none(bool),
                Range.create(
                    Position.create(
                        it.decl.loc.`range`.a.line,
                        it.decl.loc.`range`.a.character
                    ),
                    Position.create(
                        it.decl.loc.`range`.b.line,
                        it.decl.loc.`range`.b.character
                    )
                ),
                Range.create(
                    Position.create(
                        it.decl.loc.`range`.a.line,
                        it.decl.loc.`range`.a.character
                    ),
                    Position.create(
                        it.decl.loc.`range`.b.line,
                        it.decl.loc.`range`.b.character
                    )
                ),
                none(seq[DocumentSymbol])
            ).JsonNode
        if res.len == 0:
            none(JsonNode)
        else:
            some res
    of TermKind.FuncdefInst..TermKind.Meta:
        none(JsonNode)
    of TermKind.Seq:
        none(JsonNode)
    of TermKind.Us:
        none(JsonNode)
proc `textDocument/documentSymbol`(s: Stream, msg: RequestMessage, configuration: Configuration, buffers: var Buffers) =
    if msg.params.isValid(DocumentSymbolParams):
        let
            params = DocumentSymbolParams(msg.params)
            textDocument = params["textDocument"]
            uri = textDocument["uri"].getStr
        s.window.logMessage(fmt"[textDocument/documentSymbol]: {uri}")
        var data: Option[JsonNode]
        if uri in buffers.termbuf:
            let
                term = buffers.termbuf[uri]
            data = term.collectSymbol
        s.respond(msg):
            if data.isNone or data.get.len == 0:
                newJNull()
            else:
                data.get
    else:
        s.window.logMessage("[textDocument/documentSymbol]: valid params")

proc `textDocument/semanticTokens/full`(s: Stream, msg: RequestMessage, configuration: Configuration, buffers: var Buffers) =
    let
        tokenTypes = configuration.tokenTypes
        tokenModifiers = configuration.tokenModifiers
    proc index(s: seq[string], str: string): int =
        for (i, e) in s.pairs:
            if e == str:
                return i
        return -1
    proc coloring(self: Term, data: var seq[(int, int, int, int, int)])
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
    proc coloring(self: FunctionParam, data: var seq[(int, int, int, int, int)]) =
        let
            gen = self.gen
            params = self.params
            rety = self.rety
        for e in gen & params:
            let
                pat = e.pat
                typ = e.typ
                default = e.default
            pat.coloring(data)
            if typ.isSome:
                typ.get.coloring(data)
            if default.isSome:
                default.get.coloring(data)
        rety.coloring(data)
    proc coloring(self: IdentDef, data: var seq[(int, int, int, int, int)]) =
        let
            iddef = self
            pat = iddef.pat
            typ = iddef.typ
            default = iddef.default
        pat.coloring(data)
        if typ.isSome:
            typ.get.coloring(data)
        if default.isSome:
            default.get.coloring(data)
    proc coloring(self: Term, data: var seq[(int, int, int, int, int)]) =
        if self.inserted:
            return
        case self.kind
        of TermKind.Failed:
            discard
        of TermKind.bottom:
            discard
        of TermKind.`()`:
            discard
        of TermKind.Unit:
            discard
        of TermKind.U:
            discard
        # of TermKind.Bool:
        #     discard
        of TermKind.Integer, TermKind.Float:
            let
                kind = SemanticTokenTypes.number
                loc = self.loc
                a = loc.`range`.a
                b = loc.`range`.b
                l = b.character - a.character
            assert a.line == b.line
            data.add (a.line, a.character, l, tokenTypes.index($kind), 0)
        of TermKind.Char:
            discard
        of TermKind.String:
            discard
        of TermKind.Id:
            if not self.typ.isNil and self.typ.symbol.isSome:
                let
                    symbol = self.typ.symbol.get
                    loc = self.loc
                    a = loc.`range`.a
                    b = loc.`range`.b
                    l = b.character - a.character
                    kind = case symbol.kind
                        of il.SymbolKind.Var:
                            SemanticTokenTypes.variable
                        of il.SymbolKind.Let:
                            SemanticTokenTypes.variable
                        of il.SymbolKind.Const:
                            SemanticTokenTypes.variable
                        of il.SymbolKind.Typ:
                            SemanticTokenTypes.type
                        of il.SymbolKind.Func:
                            SemanticTokenTypes.function
                assert a.line == b.line, $a.line
                data.add (a.line, a.character, l, tokenTypes.index($kind), 0)
            else:
                s.window.logMessage(fmt"{self} at {self.loc}")
        # of TermKind.Lambda:
        #     discard
        # of TermKind.List:
        #     discard
        of TermKind.Tuple:
            for e in self.terms:
                e.coloring(data)
        of TermKind.Record:
            discard  # named tuple
        of TermKind.Let, TermKind.Const:
            for e in self.iddefs:
                e.coloring(data)
        # of TermKind.Typedef:
        #     discard
        of TermKind.Funcdef:
            let
                fn = self.fn
                id = fn.id
                param = fn.param
                metadata = fn.metadata
                body = fn.body
            id.coloring(data)
            param.coloring(data)
            # TODO: metadata
            body.term.coloring(data)
        of TermKind.FuncdefInst:
            discard
        of TermKind.FunctionInst:
            self.pfn.coloring(data)
            for e in self.instargs:
                e.coloring(data)
        # of TermKind.If:
        #     discard
        # of TermKind.When:
        #     discard
        of TermKind.Case:
            discard
        # of TermKind.While:
        #     discard
        # of TermKind.For:
        #     discard
        # of TermKind.Loop:
        #     discard
        # of TermKind.Block:
        #     discard
        # of TermKind.Asign:
        #     discard
        of TermKind.Typeof, TermKind.Discard:
            self.term.coloring(data)
        of TermKind.Apply:
            self.callee.coloring(data)
            for e in self.args:
                e.coloring(data)
        of TermKind.Meta:
            discard
        of TermKind.Seq:
            # self.terms.applyIt(it.coloring(data))
            for e in self.terms:
                e.coloring(data)
        of TermKind.Us:
            discard
    if msg.params.isValid(SemanticTokensParams):
        let
            params = SemanticTokensParams(msg.params)
            textDocument = params["textDocument"]
            uri = textDocument["uri"].getStr
        s.window.logMessage("[textDocument/semanticTokens/full] " & uri)
        var data: seq[int] = @[]
        if uri in buffers.termbuf:
            let
                term = buffers.termbuf[uri] # main function
            var
                data2: seq[(int, int, int, int, int)] = @[]
                prevLine = 0
                prevCh = 0
            term.fn.body.term.coloring(data2)
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
proc toDiags(errs: seq[ParseError]): seq[Diagnostic] =
    if errs.len == 0:
        return @[]
    errs.mapIt(
        Diagnostic.create(
            Range.create(
                Position.create(it.loc.`range`.a.line, it.loc.`range`.a.character),
                Position.create(it.loc.`range`.a.line, it.loc.`range`.a.character+1)
            ),
            some DiagnosticSeverity.Error.int,
            none int,
            none CodeDescription,
            none string,
            $it,
            none seq[int],
            none seq[DiagnosticRelatedInformation],
            none JsonNode
        )
    )
proc `textDocument/didOpen`(s: Stream, params: JsonNode, buffers: var Buffers) =
    if params.isValid(DidOpenTextDocumentParams):
        let
            params = DidOpenTextDocumentParams(params)
            textDocument = params["textDocument"]
            uri = textDocument["uri"].getStr
            text = textDocument["text"].getStr
        s.window.logMessage(fmt"Got didOpen notificfation {uri}")
        s.window.logMessage(fmt"Got didOpen notificfation {uri}")
        let
            parser = newParser()
            module = newModule()
            res = parser.parse(uri, text)
            (term, errs) = res.sema(module)
        buffers.astbuf[uri] = res
        buffers.termbuf[uri] = term
        buffers.modbuf[uri] = module
        s.window.logMessage(fmt"Got didOpen notificfation {uri}")

        var diags = parser.errs.toDiags
        s.textDocument(uri).publishDiagnostics(diags)
        s.window.logMessage(fmt"Got didOpen notificfation {uri}")
proc `textDocument/didChange`(s: Stream, params: JsonNode, buffers: var Buffers) =
    if params.isValid(DidChangeTextDocumentParams):
        let
            params = DidChangeTextDocumentParams(params)
            contentChanges = params["contentChanges"]
            textDocument = params["textDocument"]
            uri = textDocument["uri"].getStr
            text = contentChanges[0]["text"].getStr
        s.window.logMessage(fmt"Got didChange notificfation: {uri}")
        let
            parser = newParser()
            module = newModule()
            res = parser.parse(uri, text)
            (term, errs) = res.sema(module)
        buffers.astbuf[uri] = res
        buffers.termbuf[uri] = term
        buffers.modbuf[uri] = module

        var diags = parser.errs.toDiags
        s.textDocument(uri).publishDiagnostics(diags)
proc Lsp*(): int =
    let
        instream = stdin.newFileStream
        outstream = stdout.newFileStream
    var
        configuration = Configuration()
        buffers = newBuffers()
    while true:
        let
            msg = instream.readMessage
        if msg.isValid(RequestMessage):
            let
                msg = RequestMessage(msg)
                `method` = msg.`method`
            case `method`
            of "initialize":
                outstream.initilize(msg, configuration, buffers)
            of "shutdown":
                outstream.respond(msg):
                    newJNull()
            of "textDocument/hover":
                outstream.`textDocument/hover`(msg, configuration, buffers)
            of "textDocument/documentSymbol":
                outstream.`textDocument/documentSymbol`(msg, configuration, buffers)
            of "textDocument/semanticTokens/full":
                outstream.`textDocument/semanticTokens/full`(msg, configuration, buffers)
        elif msg.isValid(NotificationMessage):
            let
                msg = NotificationMessage(msg)
                `method` = msg.`method`
                params = msg.params
            case `method`
            of "initialized":
                outstream.window.logMessage("initilized.")
            of "exit":
                break
            of "textDocument/didOpen":
                outstream.`textDocument/didOpen`(params, buffers)
            of "textDocument/didChange":
                outstream.`textDocument/didChange`(params, buffers)

    0

when isMainModule:
    let
        parser = newParser()
    echo parser.parse("test/test04.rgn")
    for err in parser.errs:
        echo err
    echo parser.errs.len
