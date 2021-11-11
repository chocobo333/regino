
import sequtils
import strutils
import strformat
import streams
import options
import sugar
import json

import jsonschema

import eat


type
    IllformedError* = object of CatchableError
    TextDocumentSyncKind = enum
        None = 0
        Full = 1
        Incremental = 2
    DiagnosticSeverity = enum
        Error = 1
        Warning = 2
        Information = 3
        Hint = 4
    DiagnosticTag = enum
        Unnecessary = 1
        Deprecated = 2
    MessageType {.pure.} = enum
        Error = 1
        Warning = 2
        Info = 3
        Log = 4

jsonSchema:
    Message:
        jsonrpc: int or float or string
    RequestMessage extends Message:
        id: int or string
        "method": string
        params ?: any
    ResponseMessage extends Message:
        id: int or string or nil
        "result" ?: any
        error ?: ResponseError
    ResponseError:
        code: int
        message: string
        data ?: any
    NotificationMessage extends Message:
        "method": string
        params ?: any
    Position:
        line: int
        character: int
    Range:
        start: Position
        "end": Position
    Location:
        uri: string
        "range": Range
    CodeDescription:
        href: string
    DiagnosticRelatedInformation:
        location: Location
        message: string
    Diagnostic:
        "range": Range
        severity ?: int
        code ?: int or string
        codeDescription ?: CodeDescription
        source ?: string
        message : string
        tags ?: int[]
        relatedInformation ?: DiagnosticRelatedInformation[]
        data ?: any
    TextDocumentIdentifier:
        uri: string
    VersionedTextDocumentIdentifier extends TextDocumentIdentifier:
        version: int
    TextDocumentContentChangeEvent:
        "range" ?: Range
        rangeLength ?: int
        text: string
    TextDocumentItem:
        uri: string
        languageId: string
        version: int or float
        text: string
    ShowMessageParams:
        "type": int # MessageType
        message: string
    DidOpenTextDocumentParams:
        textDocument: TextDocumentItem
    DidChangeTextDocumentParams:
        textDocument: VersionedTextDocumentIdentifier
        contentChanges: TextDocumentContentChangeEvent[]
    TextDocumentSyncOptions:
        openClose ?: bool
        change ?: int
    # FileOperations:
    #     didCreate ?: FileOperationRegistrationOptions
    #     willCreate ?: FileOperationRegistrationOptions
    #     didRename ?: FileOperationRegistrationOptions
    #     willRename ?: FileOperationRegistrationOptions
    #     didDelete ?: FileOperationRegistrationOptions
    #     willDelete ?: FileOperationRegistrationOptions
    # Workspace:
    #     workspaceFolders ?: WorkspaceFoldersServerCapabilities
    #     fileOperations ?: FileOperations


    ServerCapabilities:
        textDocumentSync ?: TextDocumentSyncOptions or int
        # completionProvider ?: CompletionOptions
        # hoverProvider ?: bool or HoverOptions
        # signatureHelpProvider ?: SignatureHelpOptions
        # declarationProvider ?: bool or DeclarationOptions or DeclarationRegistrationOptions
        # definitionProvider ?: bool or DefinitionOptions
        # typeDefinitionProvider ?: bool or TypeDefinitionOptions or TypeDefinitionRegistrationOptions
        # implementationProvider ?: bool or ImplementationOptions or ImplementationRegistrationOptions
        # referencesProvider ?: bool or ReferenceOptions
        # documentHighlightProvider ?: bool or DocumentHighlightOptions
        # documentSymbolProvider ?: bool or DocumentSymbolOptions
        # codeActionProvider ?: bool or CodeActionOptions
        # codeLensProvider ?: CodeLensOptions
        # documentLinkProvider ?: DocumentLinkOptions
        # colorProvider ?: bool or DocumentColorOptions or DocumentColorRegistrationOptions
        # documentFormattingProvider ?: bool or DocumentFormattingOptions
        # documentRangeFormattingProvider ?: bool or DocumentRangeFormattingOptions
        # documentOnTypeFormattingProvider ?: DocumentOnTypeFormattingOptions
        # renameProvider ?: bool or RenameOptions
        # foldingRangeProvider ?: bool or FoldingRangeOptions or FoldingRangeRegistrationOptions
        # executeCommandProvider ?: ExecuteCommandOptions
        # selectionRangeProvider ?: bool or SelectionRangeOptions or SelectionRangeRegistrationOptions
        # linkedEditingRangeProvider ?: bool or LinkedEditingRangeOptions or LinkedEditingRangeRegistrationOptions
        # callHierarchyProvider ?: bool or CallHierarchyOptions or CallHierarchyRegistrationOptions
        # semanticTokensProvider ?: SemanticTokensOptions or SemanticTokensRegistrationOptions
        # monikerProvider ?: bool or MonikerOptions or MonikerRegistrationOptions
        # workspaceSymbolProvider ?: bool or WorkspaceSymbolOptions
        # workspace ?: Workspace
        # experimental?: any
    InitializeResult:
        capabilities: ServerCapabilities

let
    sp0 = sp(0)
    number = p"[0-9]+"
    untilcolon = p".*?:"

let
    jsonrpc = "2.0"

proc readMessage*(s: Stream): JsonNode =
    var
        contentLength: int = -1
        line: string
    while true:
        line = s.readLine
        if line.len != 0:
            break
    while true:
        let span = terminated(untilcolon, sp0)(line)
        case span.get.fragment
        of "Content-Length:":
            contentLength = (number @ (it => it.fragment.parseInt))(span.getSrc.fragment).get
        else:
            discard
        line = s.readLine
        if line.len == 0:
            break
    if contentLength != -1:
        s.readStr(contentLength).parseJson
    else:
        raise newException(IllformedError, "Missing Content-Length header")

proc sendMessage(s: Stream, n: JsonNode) =
    var msg = newStringOfCap(1024)
    toUgly(msg, n)
    s.write &"Content-Length: {msg.len}\r\n\r\n{msg}"
    s.flush

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
proc notify(s: Stream, `method`: string) =
    s.sendMessage NotificationMessage.create(jsonrpc, `method`, none JsonNode).JsonNode
proc notify(s: Stream, `method`: string, params: JsonNode) =
    s.sendMessage NotificationMessage.create(jsonrpc, `method`, some params).JsonNode
proc `textDocument/didOpen`(s: Stream, params: JsonNode) =
    if params.isValid(DidOpenTextDocumentParams):
        let
            params = DidOpenTextDocumentParams(params)
        s.notify(
            "window/showMessage",
            ShowMessageParams.create(
                MessageType.Log.int,
                "Got didOpen notificfation"
            ).JsonNode
        )
proc `textDocument/didChange`(s: Stream, params: JsonNode) =
    if params.isValid(DidChangeTextDocumentParams):
        let
            params = DidChangeTextDocumentParams(params)
        s.notify(
            "window/showMessage",
            ShowMessageParams.create(
                MessageType.Log.int,
                "Got didChange notificfation"
            ).JsonNode
        )
proc Lsp*(): int =
    let
        instream = stdin.newFileStream
        outstream = stdout.newFileStream
    while true:
        let
            msg = instream.readMessage
        if msg.isValid(RequestMessage):
            let
                msg = RequestMessage(msg)
                `method` = msg.`method`
            proc respond(s: Stream, n: JsonNode) =
                s.sendMessage msg.response(n)
            case `method`
            of "initialize":
                outstream.respond InitializeResult.create(
                    ServerCapabilities.create(
                        some TextDocumentSyncOptions.create(
                            some true,
                            some TextDocumentSyncKind.Full.int
                        )
                    )
                ).JsonNode
            of "shutdown":
                outstream.respond newJNull()
        elif msg.isValid(NotificationMessage):
            let
                msg = NotificationMessage(msg)
                `method` = msg.`method`
                params = msg.params
            case `method`
            of "initialized":
                discard
            of "exit":
                break
            of "textDocument/didOpen":
                outstream.`textDocument/didOpen`(params)
            of "textDocument/didChange":
                outstream.`textDocument/didChange`(params)

    0

when isMainModule:
    let
        instream = stdin.newFileStream
        msg = instream.readMessage
    echo msg.isValid(RequestMessage)
    echo msg
