
import tables

import ../il
import ../parsers
import ../errors
# import codegen


type
    Buffer*[T] = TableRef[string, T]
    Project* = ref object
        main*: string
        src*: Buffer[ref Source]
        program*: Buffer[il.Program]
        terrs*: Buffer[seq[TypeError]]

