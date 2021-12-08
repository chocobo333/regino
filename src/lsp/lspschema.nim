
import options
import sequtils
import json
import jsonschema


type
    IllformedError* = object of CatchableError
    TextDocumentSyncKind* = enum
        None = 0
        Full = 1
        Incremental = 2
    DiagnosticSeverity* = enum
        Error = 1
        Warning = 2
        Information = 3
        Hint = 4
    DiagnosticTag* = enum
        Unnecessary = 1
        Deprecated = 2
    MessageType* {.pure.} = enum
        Error = 1
        Warning = 2
        Info = 3
        Log = 4
    SymbolKind* {.pure.} = enum
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
    SymbolTag* {.pure.} = enum
        Deprecated = 1
    ResourceOperationKind* {.pure.} = enum
        Create = "create"
        Rename = "rename"
        Delete = "delete"
    FailureHandlingKind* {.pure.} = enum
        Abort = "abort"
        Transactional = "transactional"
        TextOnlyTransactional = "textOnlyTransactional"
        Undo = "undo"
    MarkupKind* {.pure.} = enum
        PlainText = "plaintext"
        Markdown = "markdown"
    CompletionItemTag* {.pure.} = enum
        Deprecated = 1
    InsertTextMode* {.pure.} = enum
        asIs = 1
        adjustIndentation = 2
    CompletionItemKind* {.pure.} = enum
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
    CodeActionKind* {.pure.} = enum
        Empty = ""
        QuickFix = "quickfix"
        Refactor = "refactor"
        RefactorExtract = "refactor.extract"
        RefactorInline = "refactor.inline"
        RefactorRewrite = "refactor.rewrite"
        Source = "source"
        SourceOrganizeImports = "source.organizeImports"
        SourceFixAll = "source.fixAll"
    PrepareSupportDefaultBehavior* {.pure.} = enum
        Identifier = 1
    TokenFormat* {.pure.} = enum
        Relative = "relative"
    SemanticTokenTypes* {.pure.} = enum
        namespace = "namespace"
        type = "type"
        class = "class"
        `enum` = "enum"
        `interface` = "interface"
        struct = "struct"
        typeParameter = "typeParameter"
        parameter = "parameter"
        variable = "variable"
        property = "property"
        enumMember = "enumMember"
        event = "event"
        function = "function"
        `method` = "method"
        `macro` = "macro"
        keyword = "keyword"
        modifier = "modifier"
        comment = "comment"
        string = "string"
        number = "number"
        regexp = "regexp"
        operator = "operator"
    SemanticTokenModifiers {.pure.} = enum
        declaration = "declaration"
        definition = "definition"
        readonly = "readonly"
        static = "static"
        deprecated = "deprecated"
        abstract = "abstract"
        async = "async"
        modification = "modification"
        documentation = "documentation"
        defaultLibrary = "defaultLibrary"

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
    WorkDoneProgressParams:
        workDoneToken ?: int or string
    PartialResultParams:
        partialResultToken ?: int or string # ProgressToken
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
    SemanticTokensParams extends WorkDoneProgressParams:
        partialResultToken ?: int or string # ProgressToken
        textDocument: TextDocumentIdentifier
    SemanticTokens:
        resultId ?: string
        data: int[] # uint
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

    WorkDoneProgressOptions:
        workDoneProgress ?: bool

    SemanticTokensLegend:
        tokenTypes: string[]
        tokenModifiers: string[]
    SemanticTokensOptions extends WorkDoneProgressOptions:
        legend: SemanticTokensLegend
        "range" ?: bool or nil
        full ?: bool or SemanticTokensDelta

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
        semanticTokensProvider ?: SemanticTokensOptions # or SemanticTokensRegistrationOptions
        # monikerProvider ?: bool or MonikerOptions or MonikerRegistrationOptions
        # workspaceSymbolProvider ?: bool or WorkspaceSymbolOptions
        # workspace ?: Workspace
        # experimental?: any
    ServerInfo:
        name: string
        version ?: string
    InitializeResult:
        capabilities: ServerCapabilities
        serverInfo ?: ServerInfo

export
    unsafeOptAccess,
    unsafeAccess,
    `[]`,
    isValid,
    create
export
    Message,
    RequestMessage,
    ResponseMessage,
    ResponseError,
    NotificationMessage,
    Position,
    Range,
    Location,
    CodeDescription,
    DiagnosticRelatedInformation,
    Diagnostic,
    TextDocumentIdentifier,
    VersionedTextDocumentIdentifier,
    TextDocumentContentChangeEvent,
    TextDocumentItem,
    ShowMessageParams,
    LogMessageParams,
    DidOpenTextDocumentParams,
    DidChangeTextDocumentParams,
    SemanticTokensParams,
    SemanticTokens,
    PublishDiagnosticsParams,
    TextDocumentSyncOptions,
    WorkDoneProgressParams,
    ChangeAnnotationSupport,
    WorkspaceEditClientCapabilities,
    DidChangeConfigurationClientCapabilities,
    DidChangeWatchedFilesClientCapabilities,
    SymbolKindCapability,
    SymbolTagSupport,
    ResolveSupport,
    WorkspaceSymbolClientCapabilities,
    ExecuteCommandClientCapabilities,
    SemanticTokensWorkspaceClientCapabilities,
    CodeLensWorkspaceClientCapabilities,
    FileOperations,
    WorkspaceClientCapabilities,
    TextDocumentSyncClientCapabilities,
    CompletionItemTagSupport,
    InsertTextModeSupport,
    CompletionItemCapability,
    CompletionItemKindCapability,
    CompletionList,
    CompletionClientCapabilities,
    HoverClientCapabilities,
    ParameterInformation,
    SignatureInformation,
    SignatureHelpClientCapabilities,
    DeclarationClientCapabilities,
    DefinitionClientCapabilities,
    TypeDefinitionClientCapabilities,
    ImplementationClientCapabilities,
    ReferenceClientCapabilities,
    DocumentHighlightClientCapabilities,
    DocumentSymbolClientCapabilities,
    CodeActionKindCapability,
    CodeActionLiteralSupport,
    CodeActionClientCapabilities,
    CodeLensClientCapabilities,
    DocumentLinkClientCapabilities,
    DocumentColorClientCapabilities,
    DocumentFormattingClientCapabilities,
    DocumentRangeFormattingClientCapabilities,
    DocumentOnTypeFormattingClientCapabilities,
    RenameClientCapabilities,
    DiagnosticTagSupport,
    PublishDiagnosticsClientCapabilities,
    FoldingRangeClientCapabilities,
    SelectionRangeClientCapabilities,
    LinkedEditingRangeClientCapabilities,
    CallHierarchyClientCapabilities,
    SemanticTokensDelta,
    SemanticTokensRequest,
    SemanticTokensClientCapabilitie,
    MonikerClientCapabilities,
    TextDocumentClientCapabilities,
    MessageActionItem,
    ShowMessageRequestClientCapabilities,
    ShowDocumentClientCapabilities,
    WindowClientCapabilities,
    StaleRequestSupport,
    RegularExpressionsClientCapabilities,
    MarkdownClientCapabilities,
    GeneralClientCapabilities,
    ClientCapabilities,
    WorkspaceFolder,
    ClientInfo,
    InitializeParams,
    WorkDoneProgressOptions,
    SemanticTokensLegend,
    SemanticTokensOptions,
    ServerCapabilities,
    ServerInfo,
    InitializeResult
