
import options

import il

proc newIddefSection*(iddefs: seq[IdentDef], comments: seq[Comment]): IdentDefSection =
    IdentDefSection(iddefs: iddefs, comments: comments)
proc newTypedefSection*(typedefs: seq[TypeDef], comments: seq[Comment]): TypeDefSection =
    TypeDefSection(typedefs: typedefs, comments: comments)
 