
# import compiler
import v2/lsp

import cligen


when isMainModule:
    # dispatchMulti([compile], [Lsp, cmdName="lsp"])
    dispatchMulti([Lsp, cmdName="lsp"])
