
import json
import options

import lspschema


type
    DocumentSymbolClientConfiguration* = object
        symbolKinds: set[SymbolKind]
        supportedTags: set[SymbolTag]
        labelSupport: bool
    Configuration* = object
        tokenTypes*: seq[string]
        tokenModifiers*: seq[string]
        documentSymbol*: DocumentSymbolClientConfiguration

converter to*(self: DocumentSymbolClientCapabilities): DocumentSymbolClientConfiguration =
    let
        symbolKinds: set[SymbolKind] = block:
            var res: set[SymbolKind]
            let sk = self["symbolKind"]
            if sk.isSome:
                if sk.get.hasKey("valueSet"):
                    for e in sk.get["valueSet"].to(seq[int]):
                        res.incl e.SymbolKind
                else:
                    res = {SymbolKind.File..SymbolKind.Array}
            res
        symbolTags: set[SymbolTag] = block:
            var res: set[SymbolTag]
            let ts = self["tagSupport"]
            if ts.isSome:
                if ts.get.hasKey("valueSet"):
                    for e in ts.get["valueSet"].to(seq[int]):
                        res.incl e.SymbolTag
            res
        labelSupport: bool = block:
            let ls = self["labelSupport"]
            if ls.isSome:
                ls.get.getBool
            else:
                false

    DocumentSymbolClientConfiguration(
        symbolKinds: symbolKinds,
        supportedTags: symbolTags,
        labelSupport: labelSupport
    )
