
import
    lsp,
    compiler

import cligen


when isMainModule:
    dispatchMulti([compile], [Lsp, cmdName="lsp"])
