
import compiler
import lsp

import cligen


when isMainModule:
    dispatchMulti([compile], [Lsp, cmdName="lsp"])
