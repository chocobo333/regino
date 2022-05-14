
import
    lsp,
    compiler

import cligen


const version = "0.1.0"
when isMainModule:
    clCfg.version = version
    dispatchMulti([compile, cmdName="c"], [Lsp, cmdName="lsp"])
