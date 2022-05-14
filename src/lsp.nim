
import sequtils
import strutils
import strformat
import streams
import json
import tables
import sets
import options

import lsp/[
    lspschema,
    lspprotocol,
    configuration,
    lsputils,
    window,
    textDocument/semanticTokens
]
import
    il,
    parsers,
    sema,
    typeenv,
    # codegen,
    utils,
    errors
import lineinfos except Position, Location
type rPosition = lineinfos.Position
import projects


type
    TextDocument = object
        s: Stream
        uri: string

proc textDocument(s: Stream, uri: string): TextDocument = TextDocument(s: s, uri: uri)

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

const
    trrigerCharacters = @[".", "$", "*", "+", "-", "@", "^", "&", "|", "\\", "/", "~", "=", "%", "!", "<", ">", "?"]
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
                    some CompletionOptions.create(
                        none bool,
                        some trrigerCharacters,
                        none seq[string],
                        none bool,
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

proc collectCompletion(self: Scope, pos: rPosition): seq[Symbol] =
    var
        keys: HashSet[string]
    for e in self:
        for key in e.syms.keys:
            keys.incl key
    for key in keys:
        result.add self.lookupId(key, pos)
proc `textDocument/completion`(s: Stream, msg: RequestMessage, configuration: Configuration, project: Project) =
    if msg.params.isValid(CompletionParams):
        let
            params = CompletionParams(msg.params)
            textDocument = params["textDocument"]
            pos = params["position"].to(rPosition)
            uri = textDocument["uri"].getStr
        var res = newJArray()
        if uri in project.program:
            let
                program = project.program[uri] # main function
                scopes = scope(program, pos)
                syms = scopes.collectCompletion(pos)
            for sym in syms:
                let
                    label: string = sym.id.name
                    kind: CompletionItemKind = sym.kind
                    detail: string = $sym.typ
                res.add CompletionItem.create(
                    label,
                    some kind.int,
                    none seq[int],
                    some detail,
                    none string,
                    none bool,
                    none bool,
                    none string,
                    none string,
                    none string,
                    none int,
                    none int,
                    none TextEdit,
                    none seq[TextEdit],
                    none seq[string],
                    none lspschema.Command,
                    none JsonNode
                ).JsonNode
        s.respond(msg):
            if res.len == 0:
                newJNull()
            else:
                res
    else:
        s.window.logMessage("[textDocument/hover]: valid params")
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
                if not focus.typ.isNil and focus.typ.symbol.isSome:
                    data.add $focus.typ.symbol.get
                    if focus.typ.symbol.get.kind == il.SymbolKind.Func:
                        data.add focus.typ.symbol.get.decl_funcdef.docStr.join("\n")
                data.add fmt"{focus.name}: {focus.typ}"
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

        var diags = project.perrs(uri).toDiags & project.terrs(uri).toDiags
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

        var diags = project.perrs(uri).toDiags & project.terrs(uri).toDiags
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
            outstream.window.logMessage(`method`)
            case `method`
            of "initialize":
                outstream.initialize(msg, configuration, project)
            of "shutdown":
                outstream.respond(msg):
                    newJNull()
            of "textDocument/completion":
                outstream.`textDocument/completion`(msg, configuration, project)
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
            of "textDocument/semanticTokens/full":
                outstream.`textDocument/semanticTokens/full`(msg, configuration, project)
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
