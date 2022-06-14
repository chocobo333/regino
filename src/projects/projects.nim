
import tables

import ../il
import ../parsers
import ../errors
# import codegen


type
    Buffer*[T] = Table[string, T]
    Project* = ref object
        main*: string
        src*: Buffer[ref Source]
        program*: Buffer[il.Program]
        Terrs*: Buffer[seq[TypeError]]
