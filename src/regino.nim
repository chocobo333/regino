
import v2/[
    lsp,
    compiler
]

import cligen


when isMainModule:
    dispatchMulti([compile], [Lsp, cmdName="lsp"])
