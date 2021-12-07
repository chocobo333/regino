
import sequtils
import strutils
import strformat
import streams
import json
import tables
import options

import
    lspschema,
    lspprotocol
import
    parsers,
    sema,
    codegen
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

proc initilize(s: Stream, msg: RequestMessage) =
    if msg.params.isValid(InitializeParams, allowExtra=true):
        let
            params = InitializeParams(msg.params)
            semanticTokens = params["capabilities"]["textDocument"]["semanticTokens"]
            tokenTypes = semanticTokens["tokenTypes"]
            tokenModifiers = semanticTokens["tokenModifiers"]
        s.window.logMessage("Got initilize request")
        s.window.logMessage($tokenTypes)
        s.window.logMessage($tokenModifiers)
        s.respond(msg):
            InitializeResult.create(
                ServerCapabilities.create(
                    some TextDocumentSyncOptions.create(
                        some true,
                        some TextDocumentSyncKind.Full.int
                    )
                )
            ).JsonNode
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
        let
            parser = newParser()
            module = newModule()
            res = parser.parse(uri, text)
            # (term, errs) = res.sema(module)
        buffers.astbuf[uri] = res
        # buffers.termbuf[uri] = term

        var diags = parser.errs.toDiags
        s.textDocument(uri).publishDiagnostics(diags)
proc `textDocument/didChange`(s: Stream, params: JsonNode, buffers: var Buffers) =
    if params.isValid(DidChangeTextDocumentParams):
        let
            params = DidChangeTextDocumentParams(params)
            contentChanges = params["contentChanges"]
            textDocument = params["textDocument"]
            uri = textDocument["uri"].getStr
            text = contentChanges[0]["text"].getStr
            parser = newParser()
            res = parser.parse(uri, text)
        s.window.logMessage(fmt"Got didChange notificfation: {uri}")
        s.window.logMessage($contentChanges.JsonNode)
        buffers.astbuf[uri] = res
        let
            diags = parser.errs.toDiags
        if diags.len > 0:
            s.textDocument(uri).publishDiagnostics(diags)
proc Lsp*(): int =
    let
        instream = stdin.newFileStream
        outstream = stdout.newFileStream
    var
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
                outstream.initilize(msg)
            of "shutdown":
                outstream.respond(msg):
                    newJNull()
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
