
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
    configuration,
    lsputils
]
import
    il,
    parsers,
    sema,
    # codegen,
    utils,
    errors
import lineinfos except Position, Location
type rPosition = lineinfos.Position
import projects


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

proc initialize(s: Stream, msg: RequestMessage, configuration: var Configuration, project: Project) =
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
        s.window.logMessage("Got initialize request")
        s.respond(msg):
            InitializeResult.create(
                ServerCapabilities.create(
                    some TextDocumentSyncOptions.create(
                        some true,
                        some TextDocumentSyncKind.Full.int
                    ),
                    some true, # Hover
                    some true, # declaration
                    some true, # definition
                    some true, # reference
                    some true, # documentHighlight
                    some DocumentSymbolOptions.create(
                        none(bool),
                        some "regino"
                    ),
                    # some SemanticTokensOptions.create(
                    #     none(bool),
                    #     SemanticTokensLegend.create(
                    #         tokenTypes,
                    #         tokenModifiers
                    #     ),
                    #     some false,
                    #     some true
                    # ),
                    none SemanticTokensOptions,

                ),
                some ServerInfo.create(
                    "regino-languages-server",
                    some "0.1.0"
                )
            ).JsonNode

proc `textDocument/hover`(s: Stream, msg: RequestMessage, configuration: Configuration, project: Project) =
    if msg.params.isValid(HoverParams):
        let
            params = HoverParams(msg.params)
            textDocument = params["textDocument"]
            pos = params["position"].to(rPosition)
            uri = textDocument["uri"].getStr
        var data: seq[string]
        if uri in project.program:
            let
                program = project.program[uri] # main function
                focus = program.find(pos)
            if focus.isSome:
                let focus = focus.get
                if focus.typ.symbol.isSome:
                    data.add $focus.typ.symbol.get
                data.add fmt"{focus.name}: {focus.typ}"
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
proc `textDocument/declaration`(s: Stream, msg: RequestMessage, configuration: Configuration, project: Project) =
    if msg.params.isValid(DeclarationParams):
        let
            params = DeclarationParams(msg.params)
            textDocument = params["textDocument"]
            pos = params["position"].to(rPosition)
            uri = textDocument["uri"].getStr
        if uri in project.program:
            let
                program = project.program[uri] # main function
                focus = program.find(pos)
            if focus.isSome and focus.get.typ.symbol.isSome:
                let
                    loc = focus.get.typ.symbol.get.loc
                s.respond(msg):
                    loc.to.JsonNode
                return
        s.respond(msg):
            newJNull()
    else:
        s.window.logMessage("[textDocument/declaration]: valid params")
proc `textDocument/definition`(s: Stream, msg: RequestMessage, configuration: Configuration, project: Project) =
    if msg.params.isValid(DefinitionParams):
        let
            params = DefinitionParams(msg.params)
            textDocument = params["textDocument"]
            pos = params["position"].to(rPosition)
            uri = textDocument["uri"].getStr
        if uri in project.program:
            let
                program = project.program[uri] # main function
                focus = program.find(pos)
            if focus.isSome and focus.get.typ.symbol.isSome:
                let
                    loc = focus.get.typ.symbol.get.loc
                s.respond(msg):
                    loc.to.JsonNode
                return
        s.respond(msg):
            newJNull()
    else:
        s.window.logMessage("[textDocument/definition]: valid params")
proc `textDocument/references`(s: Stream, msg: RequestMessage, configuration: Configuration, project: Project) =
    if msg.params.isValid(ReferenceParams):
        let
            params = ReferenceParams(msg.params)
            textDocument = params["textDocument"]
            pos = params["position"].to(rPosition)
            uri = textDocument["uri"].getStr
        var res = newJArray()
        if uri in project.program:
            let
                program = project.program[uri] # main function
                focus = program.find(pos)
            if focus.isSome and focus.get.typ.symbol.isSome:
                let
                    use = focus.get.typ.symbol.get.use
                for loc in use:
                    res.add loc.to.JsonNode
                s.respond(msg):
                    res
                return
        s.respond(msg):
            newJNull()
    else:
        s.window.logMessage("[textDocument/references]: valid params")
        s.window.logMessage("[textDocument/definition]: valid params")
proc `textDocument/documentHighlight`(s: Stream, msg: RequestMessage, configuration: Configuration, project: Project) =
    if msg.params.isValid(DocumentHighlightParams):
        let
            params = DocumentHighlightParams(msg.params)
            textDocument = params["textDocument"]
            pos = params["position"].to(rPosition)
            uri = textDocument["uri"].getStr
        var res = newJArray()
        if uri in project.program:
            let
                program = project.program[uri] # main function
                focus = program.find(pos)
            if focus.isSome and focus.get.typ.symbol.isSome:
                let
                    loc = focus.get.typ.symbol.get.loc
                    use = focus.get.typ.symbol.get.use
                for loc in use:
                    res.add DocumentHighlight.create(
                        loc.range.to,
                        some DocumentHighlightKind.Text.int
                    ).JsonNode
                res.add DocumentHighlight.create(
                    loc.range.to,
                    some DocumentHighlightKind.Text.int
                ).JsonNode
                s.respond(msg):
                    res
                return
        s.respond(msg):
            newJNull()
    else:
        s.window.logMessage("[textDocument/documentHighlight]: valid params")
proc collectSymbol(self: Scope): Option[JsonNode] =
    var res = newJArray()
    for syms in self.syms.values:
        for sym in syms:
            let kind: lspschema.SymbolKind = sym.kind
            res.add DocumentSymbol.create(
                sym.id.name,
                some $sym.typ,
                kind.int,
                none(seq[int]),
                none(bool),
                Range.create(
                    Position.create(
                        sym.id.loc.`range`.a.line,
                        sym.id.loc.`range`.a.character
                    ),
                    Position.create(
                        sym.id.loc.`range`.b.line,
                        sym.id.loc.`range`.b.character
                    )
                ),
                Range.create(
                    Position.create(
                        sym.id.loc.`range`.a.line,
                        sym.id.loc.`range`.a.character
                    ),
                    Position.create(
                        sym.id.loc.`range`.b.line,
                        sym.id.loc.`range`.b.character
                    )
                ),
                none(seq[DocumentSymbol])
            ).JsonNode
    if res.len == 0:
        none(JsonNode)
    else:
        some res
# proc collectSymbol(self: Term): Option[JsonNode] =
#     # result type is DocumentSymbol[]
#     case self.kind
#     of TermKind.Failed..TermKind.Const:
#         none(JsonNode)
#     of TermKind.Funcdef:
#         var res = newJArray()
#         for it in toSeq(self.fn.body.scope.syms.values).flatten:
#             let
#                 kind = case it.kind:
#                 of il.SymbolKind.Var, il.SymbolKind.Let:
#                     lspschema.SymbolKind.Variable
#                 of il.SymbolKind.Const:
#                     lspschema.SymbolKind.Constant
#                 of il.SymbolKind.Typ:
#                     lspschema.SymbolKind.Class
#                 of il.SymbolKind.Func:
#                     lspschema.SymbolKind.Function
#             res.add DocumentSymbol.create(
#                 it.decl.name,
#                 some $it.typ,
#                 kind.int,
#                 none(seq[int]),
#                 none(bool),
#                 Range.create(
#                     Position.create(
#                         it.decl.loc.`range`.a.line,
#                         it.decl.loc.`range`.a.character
#                     ),
#                     Position.create(
#                         it.decl.loc.`range`.b.line,
#                         it.decl.loc.`range`.b.character
#                     )
#                 ),
#                 Range.create(
#                     Position.create(
#                         it.decl.loc.`range`.a.line,
#                         it.decl.loc.`range`.a.character
#                     ),
#                     Position.create(
#                         it.decl.loc.`range`.b.line,
#                         it.decl.loc.`range`.b.character
#                     )
#                 ),
#                 none(seq[DocumentSymbol])
#             ).JsonNode
#         if res.len == 0:
#             none(JsonNode)
#         else:
#             some res
#     of TermKind.FuncdefInst..TermKind.Meta:
#         none(JsonNode)
#     of TermKind.Seq:
#         none(JsonNode)
#     of TermKind.Us:
#         none(JsonNode)
proc collectSymbol(self: Program): Option[JsonNode] =
    self.scope.collectSymbol
proc `textDocument/documentSymbol`(s: Stream, msg: RequestMessage, configuration: Configuration, project: Project) =
    if msg.params.isValid(DocumentSymbolParams):
        let
            params = DocumentSymbolParams(msg.params)
            textDocument = params["textDocument"]
            uri = textDocument["uri"].getStr
        s.window.logMessage(fmt"[textDocument/documentSymbol]: {uri}")
        var data: Option[JsonNode]
        if uri in project.program:
            let
                program = project.program[uri]
            data = program.collectSymbol
        s.respond(msg):
            if data.isNone or data.get.len == 0:
                newJNull()
            else:
                data.get
    else:
        s.window.logMessage("[textDocument/documentSymbol]: valid params")

# proc `textDocument/semanticTokens/full`(s: Stream, msg: RequestMessage, configuration: Configuration, project: Project) =
#     let
#         tokenTypes = configuration.tokenTypes
#         tokenModifiers = configuration.tokenModifiers
#     proc index(s: seq[string], str: string): int =
#         for (i, e) in s.pairs:
#             if e == str:
#                 return i
#         return -1
#     proc coloring(self: Term, data: var seq[(int, int, int, int, int)])
#     # proc coloring(self: Pattern, data: var seq[(int, int, int, int, int)]) =
#     #     case self.kind
#     #     of PatternKind.Literal:
#     #         discard
#     #     of PatternKind.Ident:
#     #         self.id.coloring(data)
#     #     # of PatternKind.Range:
#     #     #     discard
#     #     # of PatternKind.Array:
#     #     #     discard
#     #     of PatternKind.Pair:
#     #         self.first.coloring(data)
#     #         self.second.coloring(data)
#     #     # of PatternKind.Tuple:
#     #     #     discard
#     #     of PatternKind.Record:
#     #         discard
#     #     of PatternKind.Discard:
#     #         discard
#     proc coloring(self: FunctionParam, data: var seq[(int, int, int, int, int)]) =
#         let
#             gen = self.gen
#             params = self.params
#             rety = self.rety
#         for e in gen & params:
#             let
#                 pat = e.pat
#                 typ = e.typ
#                 default = e.default
#             pat.coloring(data)
#             if typ.isSome:
#                 typ.get.coloring(data)
#             if default.isSome:
#                 default.get.coloring(data)
#         rety.coloring(data)
#     proc coloring(self: IdentDef, data: var seq[(int, int, int, int, int)]) =
#         let
#             iddef = self
#             pat = iddef.pat
#             typ = iddef.typ
#             default = iddef.default
#         pat.coloring(data)
#         if typ.isSome:
#             typ.get.coloring(data)
#         if default.isSome:
#             default.get.coloring(data)
#     proc coloring(self: Term, data: var seq[(int, int, int, int, int)]) =
#         if self.inserted:
#             return
#         case self.kind
#         of TermKind.Failed:
#             discard
#         of TermKind.bottom:
#             discard
#         of TermKind.`()`:
#             discard
#         of TermKind.Unit:
#             discard
#         of TermKind.U:
#             discard
#         # of TermKind.Bool:
#         #     discard
#         of TermKind.Integer, TermKind.Float:
#             let
#                 kind = SemanticTokenTypes.number
#                 loc = self.loc
#                 a = loc.`range`.a
#                 b = loc.`range`.b
#                 l = b.character - a.character
#             assert a.line == b.line
#             data.add (a.line, a.character, l, tokenTypes.index($kind), 0)
#         of TermKind.Char:
#             discard
#         of TermKind.String:
#             discard
#         of TermKind.Id:
#             if not self.typ.isNil and self.typ.symbol.isSome:
#                 let
#                     symbol = self.typ.symbol.get
#                     loc = self.loc
#                     a = loc.`range`.a
#                     b = loc.`range`.b
#                     l = b.character - a.character
#                     kind = case symbol.kind
#                         of il.SymbolKind.Var:
#                             SemanticTokenTypes.variable
#                         of il.SymbolKind.Let:
#                             SemanticTokenTypes.variable
#                         of il.SymbolKind.Const:
#                             SemanticTokenTypes.variable
#                         of il.SymbolKind.Typ:
#                             SemanticTokenTypes.type
#                         of il.SymbolKind.Func:
#                             SemanticTokenTypes.function
#                 assert a.line == b.line, $a.line
#                 data.add (a.line, a.character, l, tokenTypes.index($kind), 0)
#             else:
#                 s.window.logMessage(fmt"{self} at {self.loc}")
#         # of TermKind.Lambda:
#         #     discard
#         # of TermKind.List:
#         #     discard
#         of TermKind.Tuple:
#             for e in self.terms:
#                 e.coloring(data)
#         of TermKind.Record:
#             discard  # named tuple
#         of TermKind.Let, TermKind.Const:
#             for e in self.iddefs:
#                 e.coloring(data)
#         # of TermKind.Typedef:
#         #     discard
#         of TermKind.Funcdef:
#             let
#                 fn = self.fn
#                 id = fn.id
#                 param = fn.param
#                 metadata = fn.metadata
#                 body = fn.body
#             id.coloring(data)
#             param.coloring(data)
#             # TODO: metadata
#             body.term.coloring(data)
#         of TermKind.FuncdefInst:
#             discard
#         of TermKind.FunctionInst:
#             self.pfn.coloring(data)
#             for e in self.instargs:
#                 e.coloring(data)
#         # of TermKind.If:
#         #     discard
#         # of TermKind.When:
#         #     discard
#         of TermKind.Case:
#             discard
#         # of TermKind.While:
#         #     discard
#         # of TermKind.For:
#         #     discard
#         # of TermKind.Loop:
#         #     discard
#         # of TermKind.Block:
#         #     discard
#         # of TermKind.Asign:
#         #     discard
#         of TermKind.Typeof, TermKind.Discard:
#             self.term.coloring(data)
#         of TermKind.Apply:
#             self.callee.coloring(data)
#             for e in self.args:
#                 e.coloring(data)
#         of TermKind.Meta:
#             discard
#         of TermKind.Seq:
#             # self.terms.applyIt(it.coloring(data))
#             for e in self.terms:
#                 e.coloring(data)
#         of TermKind.Us:
#             discard
#     if msg.params.isValid(SemanticTokensParams):
#         let
#             params = SemanticTokensParams(msg.params)
#             textDocument = params["textDocument"]
#             uri = textDocument["uri"].getStr
#         s.window.logMessage("[textDocument/semanticTokens/full] " & uri)
#         var data: seq[int] = @[]
#         if uri in project.termbuf:
#             let
#                 term = project.termbuf[uri] # main function
#             var
#                 data2: seq[(int, int, int, int, int)] = @[]
#                 prevLine = 0
#                 prevCh = 0
#             term.fn.body.term.coloring(data2)
#             for (line, ch, l, typ, mods) in data2:
#                 if typ notin 0..<tokenTypes.len:
#                     continue
#                 data.add if prevLine == line:
#                     assert ch > prevCh, fmt"{ch} > {prevCh}, line: {line}"
#                     @[0, ch - prevCh, l, typ, mods]
#                 else:
#                     assert line > prevLine, fmt"{line} > {prevLine}"
#                     @[line - prevLine, ch, l, typ, mods]
#                 prevLine = line
#                 prevCh = ch
#         s.respond(msg):
#             if data.len == 0:
#                 newJNull()
#             else:
#                 SemanticTokens.create(
#                     none(string),
#                     data
#                 ).JsonNode
#         s.window.logMessage("[textDocument/semanticTokens/full]: responded")
#     else:
#         s.window.logMessage("[textDocument/semanticTokens/full]: invalid params")
proc toDiags(errs: seq[ParseError]): seq[Diagnostic] =
    if errs.len == 0:
        return @[]
    errs.mapIt(
        Diagnostic.create(
            Range.create(
                Position.create(it.loc.`range`.a.line, it.loc.`range`.a.character),
                Position.create(it.loc.`range`.b.line, it.loc.`range`.b.character)
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
proc toDiags(errs: seq[TypeError]): seq[Diagnostic] =
    if errs.len == 0:
        return @[]
    errs.mapit(
        Diagnostic.create(
            Range.create(
                Position.create(it.loc.`range`.a.line, it.loc.`range`.a.character),
                Position.create(it.loc.`range`.b.line, it.loc.`range`.b.character)
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
proc `textDocument/didOpen`(s: Stream, params: JsonNode, project: Project) =
    if params.isValid(DidOpenTextDocumentParams):
        let
            params = DidOpenTextDocumentParams(params)
            textDocument = params["textDocument"]
            uri = textDocument["uri"].getStr
            text = textDocument["text"].getStr
        s.window.logMessage(fmt"Got didOpen notificfation {uri}")
        project[uri] = text

        var diags = project.perrs(uri).toDiags & project.terrs[uri].toDiags
        s.textDocument(uri).publishDiagnostics(diags)
        s.window.logMessage(fmt"Got didOpen notificfation {uri}")
proc `textDocument/didChange`(s: Stream, params: JsonNode, project: Project) =
    if params.isValid(DidChangeTextDocumentParams):
        let
            params = DidChangeTextDocumentParams(params)
            contentChanges = params["contentChanges"]
            textDocument = params["textDocument"]
            uri = textDocument["uri"].getStr
            text = contentChanges[0]["text"].getStr
        s.window.logMessage(fmt"Got didChange notificfation: {uri}")
        project[uri] = text

        var diags = project.perrs(uri).toDiags & project.terrs[uri].toDiags
        s.textDocument(uri).publishDiagnostics(diags)
proc Lsp*(): int =
    let
        instream = stdin.newFileStream
        outstream = stdout.newFileStream
    var
        configuration = Configuration()
        project = newproject()
    while true:
        let
            msg = instream.readMessage
        if msg.isValid(RequestMessage):
            let
                msg = RequestMessage(msg)
                `method` = msg.`method`
            case `method`
            of "initialize":
                outstream.initialize(msg, configuration, project)
            of "shutdown":
                outstream.respond(msg):
                    newJNull()
            of "textDocument/hover":
                outstream.`textDocument/hover`(msg, configuration, project)
            of "textDocument/declaration":
                outstream.`textDocument/declaration`(msg, configuration, project)
            of "textDocument/definition":
                outstream.`textDocument/definition`(msg, configuration, project)
            of "textDocument/references":
                outstream.`textDocument/references`(msg, configuration, project)
            of "textDocument/documentHighlight":
                outstream.`textDocument/documentHighlight`(msg, configuration, project)
            of "textDocument/documentSymbol":
                outstream.`textDocument/documentSymbol`(msg, configuration, project)
            # of "textDocument/semanticTokens/full":
            #     outstream.`textDocument/semanticTokens/full`(msg, configuration, project)
        elif msg.isValid(NotificationMessage):
            let
                msg = NotificationMessage(msg)
                `method` = msg.`method`
                params = msg.params
            case `method`
            of "initialized":
                outstream.window.logMessage("initialized.")
            of "exit":
                break
            of "textDocument/didOpen":
                outstream.`textDocument/didOpen`(params, project)
            of "textDocument/didChange":
                outstream.`textDocument/didChange`(params, project)

    0
