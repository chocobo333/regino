
import sequtils
import strutils
import strformat
import streams
import options
import sugar
import json
import tables

import jsonschema

import eat
import parsers
import buffers


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
    SymbolKind {.pure.} = enum
        File = 1
        Module = 2
        Namespace = 3
        Package = 4
        Class = 5
        Method = 6
        Property = 7
        Field = 8
        Constructor = 9
        Enum = 10
        Interface = 11
        Function = 12
        Variable = 13
        Constant = 14
        String = 15
        Number = 16
        Boolean = 17
        Array = 18
        Object = 19
        Key = 20
        Null = 21
        EnumMember = 22
        Struct = 23
        Event = 24
        Operator = 25
        TypeParameter = 26
    SymbolTag {.pure.} = enum
        Deprecated = 1
    ResourceOperationKind {.pure.} = enum
        Create = "create"
        Rename = "rename"
        Delete = "delete"
    FailureHandlingKind {.pure.} = enum
        Abort = "abort"
        Transactional = "transactional"
        TextOnlyTransactional = "textOnlyTransactional"
        Undo = "undo"
    MarkupKind {.pure.} = enum
        PlainText = "plaintext"
        Markdown = "markdown"
    CompletionItemTag {.pure.} = enum
        Deprecated = 1
    InsertTextMode {.pure.} = enum
        asIs = 1
        adjustIndentation = 2
    CompletionItemKind {.pure.} = enum
        Text = 1
        Method = 2
        Function = 3
        Constructor = 4
        Field = 5
        Variable = 6
        Class = 7
        Interface = 8
        Module = 9
        Property = 10
        Unit = 11
        Value = 12
        Enum = 13
        Keyword = 14
        Snippet = 15
        Color = 16
        File = 17
        Reference = 18
        Folder = 19
        EnumMember = 20
        Constant = 21
        Struct = 22
        Event = 23
        Operator = 24
        TypeParameter = 25
    CodeActionKind {.pure.} = enum
        Empty = ""
        QuickFix = "quickfix"
        Refactor = "refactor"
        RefactorExtract = "refactor.extract"
        RefactorInline = "refactor.inline"
        RefactorRewrite = "refactor.rewrite"
        Source = "source"
        SourceOrganizeImports = "source.organizeImports"
        SourceFixAll = "source.fixAll"
    PrepareSupportDefaultBehavior {.pure.} = enum
        Identifier = 1
    TokenFormat {.pure.} = enum
        Relative = "relative"

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
    LogMessageParams:
        "type": int # MessageType
        message: string
    DidOpenTextDocumentParams:
        textDocument: TextDocumentItem
    DidChangeTextDocumentParams:
        textDocument: VersionedTextDocumentIdentifier
        contentChanges: TextDocumentContentChangeEvent[]
    PublishDiagnosticsParams:
        uri: string
        version ?: int
        diagnostics: Diagnostic[]
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

    WorkDoneProgressParams:
        workDoneToken ?: int or string

    ChangeAnnotationSupport:
        groupsOnLabel ?: bool
    WorkspaceEditClientCapabilities:
        documentChanges ?: bool
        resourceOperations ?: string[] # ResourceOperationKind
        failureHandling ?: string # FailureHandlingKind
        normalizesLineEndings ?: bool
        changeAnnotationSupport ?: ChangeAnnotationSupport

    DidChangeConfigurationClientCapabilities:
        dynamicRegistration ?: bool

    DidChangeWatchedFilesClientCapabilities:
        dynamicRegistration ?: bool

    SymbolKindCapability:
        valueSet ?: int[] # SymbolKind
    SymbolTagSupport:
        valueSet: int[] # SymbolTag
    ResolveSupport:
        properties: string[]

    WorkspaceSymbolClientCapabilities:
        dynamicRegistration ?: bool
        symbolKind ?: SymbolKindCapability
        tagSupport ?: SymbolTagSupport
        resolveSupport ?: ResolveSupport

    ExecuteCommandClientCapabilities:
        dynamicRegistration ?: bool
    SemanticTokensWorkspaceClientCapabilities:
        refreshSupport ?: bool
    CodeLensWorkspaceClientCapabilities:
        refreshSupport ?: bool
    FileOperations:
        dynamicRegistration ?: bool
        didCreate ?: bool
        willCreate ?: bool
        didRename ?: bool
        willRename ?: bool
        didDelete ?: bool
        willDelete ?: bool
    WorkspaceClientCapabilities:
        applyEdit ?: bool
        workspaceEdit ?: WorkspaceEditClientCapabilities
        didChangeConfiguration ?: DidChangeConfigurationClientCapabilities
        didChangeWatchedFiles ?: DidChangeWatchedFilesClientCapabilities
        symbol ?: WorkspaceSymbolClientCapabilities
        executeCommand ?: ExecuteCommandClientCapabilities
        workspaceFolders ?: bool
        configuration ?: bool
        semanticTokens ?: SemanticTokensWorkspaceClientCapabilities
        codeLens ?: CodeLensWorkspaceClientCapabilities;
        fileOperations ?: FileOperations

    TextDocumentSyncClientCapabilities:
        dynamicRegistration ?: bool
        willSave ?: bool
        willSaveWaitUntil ?: bool
        didSave ?: bool
    CompletionItemTagSupport:
        valueSet: int[] # CompletionItemTag
    InsertTextModeSupport:
        valueSet: int[] # InsertTextMode
    CompletionItemCapability:
        snippetSupport ?: bool
        commitCharactersSupport ?: bool
        documentFormat ?: string[] # MarkupKind
        deprecatedSupport ?: bool
        preselectSupport ?: bool
        tagSupport ?: CompletionItemTagSupport
        insertReplaceSupport ?: bool
        resolveSupport ?: ResolveSupport
        insertTextModeSupport ?: InsertTextModeSupport
        labelDetailsSupport ?: bool
    CompletionItemKindCapability:
        valueSet ?: int[] # CompletionItemKind
    CompletionList:
        itemDefaults ?: string[]
    CompletionClientCapabilities:
        dynamicRegistration ?: bool
        completionItem ?: CompletionItemCapability
        completionItemKind ?: CompletionItemKindCapability
        contextSupport ?: bool
        insertTextMode ?: int # InsertTextMode
        completionList ?: CompletionList
    HoverClientCapabilities:
        dynamicRegistration ?: bool
        contentFormat ?: string[] # MarkupKind
    ParameterInformation:
        labelOffsetSupport ?: bool
    SignatureInformation:
        documentationFormat ?: string[] # MarkupKind
        parameterInformation ?: ParameterInformation
        activeParameterSupport ?: bool
    SignatureHelpClientCapabilities:
        dynamicRegistration ?: bool
        signatureInformation ?: SignatureInformation
        contextSupport ?: bool
    DeclarationClientCapabilities:
        dynamicRegistration ?: bool
        linkSupport ?: bool
    DefinitionClientCapabilities:
        dynamicRegistration ?: bool
        linkSupport ?: bool
    TypeDefinitionClientCapabilities:
        dynamicRegistration ?: bool
        linkSupport ?: bool
    ImplementationClientCapabilities:
        dynamicRegistration ?: bool
        linkSupport ?: bool
    ReferenceClientCapabilities:
        dynamicRegistration ?: bool
    DocumentHighlightClientCapabilities:
        dynamicRegistration ?: bool
    DocumentSymbolClientCapabilities:
        dynamicRegistration ?: bool
        symbolKind ?: SymbolKindCapability
        hierarchicalDocumentSymbolSupport ?: bool
        tagSupport ?: SymbolTagSupport
        labelSupport ?: bool
    CodeActionKindCapability:
        valueSet: string[] # CodeActionKind
    CodeActionLiteralSupport:
        codeActionKind: CodeActionKindCapability
    CodeActionClientCapabilities:
        dynamicRegistration ?: bool
        codeActionLiteralSupport ?: CodeActionLiteralSupport
        isPreferredSupport ?: bool
        disabledSupport ?: bool
        dataSupport ?: bool
        resolveSupport ?: ResolveSupport
        honorsChangeAnnotations ?: bool
    CodeLensClientCapabilities:
        dynamicRegistration ?: bool
    DocumentLinkClientCapabilities:
        dynamicRegistration ?: bool
        tooltipSupport ?: bool
    DocumentColorClientCapabilities:
        dynamicRegistration ?: bool
    DocumentFormattingClientCapabilities:
        dynamicRegistration ?: bool
    DocumentRangeFormattingClientCapabilities:
        dynamicRegistration ?: bool
    DocumentOnTypeFormattingClientCapabilities:
        dynamicRegistration ?: bool
    RenameClientCapabilities:
        dynamicRegistration ?: bool
        prepareSupport ?: bool
        prepareSupportDefaultBehavior ?: int # PrepareSupportDefaultBehavior
        honorsChangeAnnotations ?: bool
    DiagnosticTagSupport:
        valueSet: int[] # DiagnosticTag
    PublishDiagnosticsClientCapabilities:
        relatedInformation ?: bool
        tagSupport ?: DiagnosticTagSupport
        versionSupport ?: bool
        codeDescriptionSupport ?: bool
        dataSupport ?: bool
    FoldingRangeClientCapabilities:
        dynamicRegistration ?: bool
        rangeLimit ?: int # uint
        lineFoldingOnly ?: bool
    SelectionRangeClientCapabilities:
        dynamicRegistration ?: bool
    LinkedEditingRangeClientCapabilities:
        dynamicRegistration ?: bool
    CallHierarchyClientCapabilities:
        dynamicRegistration ?: bool
    SemanticTokensDelta:
        delta ?: bool
    SemanticTokensRequest:
        "range" ?: bool or nil
        full ?: bool or SemanticTokensDelta
    SemanticTokensClientCapabilitie:
        dynamicRegistration ?: bool
        requests: SemanticTokensRequest
        tokenTypes: string[]
        tokenModifiers: string[]
        formats: string[] # TokenFormat
        overlappingTokenSupport ?: bool
        multilineTokenSupport ?: bool
        serverCancelSupport ?: bool
        augmentsSyntaxTokens ?: bool
    MonikerClientCapabilities:
        dynamicRegistration ?: bool

    TextDocumentClientCapabilities:
        synchronization ?: TextDocumentSyncClientCapabilities
        completion ?: CompletionClientCapabilities
        hover ?: HoverClientCapabilities
        signatureHelp ?: SignatureHelpClientCapabilities
        declaration ?: DeclarationClientCapabilities
        definition ?: DefinitionClientCapabilities
        typeDefinition ?: TypeDefinitionClientCapabilities
        implementation ?: ImplementationClientCapabilities
        references ?: ReferenceClientCapabilities
        documentHighlight ?: DocumentHighlightClientCapabilities
        documentSymbol ?: DocumentSymbolClientCapabilities
        codeAction ?: CodeActionClientCapabilities
        codeLens ?: CodeLensClientCapabilities
        documentLink ?: DocumentLinkClientCapabilities
        colorProvider ?: DocumentColorClientCapabilities
        formatting ?: DocumentFormattingClientCapabilities
        rangeFormatting ?: DocumentRangeFormattingClientCapabilities
        onTypeFormatting ?: DocumentOnTypeFormattingClientCapabilities
        rename ?: RenameClientCapabilities
        publishDiagnostics ?: PublishDiagnosticsClientCapabilities
        foldingRange ?: FoldingRangeClientCapabilities
        selectionRange ?: SelectionRangeClientCapabilities
        linkedEditingRange ?: LinkedEditingRangeClientCapabilities
        callHierarchy ?: CallHierarchyClientCapabilities
        semanticTokens ?: SemanticTokensClientCapabilitie
        moniker ?: MonikerClientCapabilities
    MessageActionItem:
        additionalPropertiesSupport ?: bool
    ShowMessageRequestClientCapabilities:
        messageActionItem ?: MessageActionItem
    ShowDocumentClientCapabilities:
        support: bool
    WindowClientCapabilities:
        workDoneProgress ?: bool
        showMessage ?: ShowMessageRequestClientCapabilities
        showDocument ?: ShowDocumentClientCapabilities
    StaleRequestSupport:
        cancel: bool
        retryOnContentModified: string[]
    RegularExpressionsClientCapabilities:
        engine: string
        version ?: string
    MarkdownClientCapabilities:
        parser: string
        version ?: string
        allowedTags ?: string[]
    GeneralClientCapabilities:
        staleRequestSupport ?: StaleRequestSupport
        regularExpressions ?: RegularExpressionsClientCapabilities;
        markdown ?: MarkdownClientCapabilities
    ClientCapabilities:
        workspace ?: WorkspaceClientCapabilities
        textDocument ?: TextDocumentClientCapabilities
        window ?: WindowClientCapabilities
        general ?: GeneralClientCapabilities
        experimental ?: any
    WorkspaceFolder:
        uri: string
        name: string
    ClientInfo:
        name: string
        version ?: string
    InitializeParams extends WorkDoneProgressParams:
        processId: int or nil
        clientInfo ?: ClientInfo
        locale ?: string
        rootPath ?: string or nil
        rootUri: string or nil
        initializationOptions ?: any
        capabilities: ClientCapabilities
        trace ?: string
        workspaceFolders ?: WorkspaceFolder[] or nil

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
proc `textDocument/didOpen`(s: Stream, params: JsonNode, buffers: Buffers) =
    if params.isValid(DidOpenTextDocumentParams):
        let
            params = DidOpenTextDocumentParams(params)
            textDocument = params["textDocument"]
            uri = textDocument["uri"].getStr
            text = textDocument["text"].getStr
            parser = newParser()
            res = parser.parse(uri, text)
        s.window.showMessage(fmt"Got didOpen notificfation {uri}")
        buffers.astbuf[uri] = res
        let
            diags = parser.errs.toDiags
        s.textDocument(uri).publishDiagnostics(diags)
proc `textDocument/didChange`(s: Stream, params: JsonNode, buffers: Buffers) =
    if params.isValid(DidChangeTextDocumentParams):
        let
            params = DidChangeTextDocumentParams(params)
            contentChanges = params["contentChanges"]
            textDocument = params["textDocument"]
            uri = textDocument["uri"].getStr
            text = contentChanges[0]["text"].getStr
            parser = newParser()
            res = parser.parse(uri, text)
        s.window.showMessage(fmt"Got didChange notificfation: {uri}")
        s.window.logMessage($contentChanges.JsonNode)
        buffers.astbuf[uri] = res
        let
            diags = parser.errs.toDiags
        s.textDocument(uri).publishDiagnostics(diags)
proc Lsp*(): int =
    let
        instream = stdin.newFileStream
        outstream = stdout.newFileStream
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
