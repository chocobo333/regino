
import tables
import il
import ast


type
    Buffer[T] = ref object
        impl: Table[string, T]
    Buffers* = ref object
        termbuf*: Buffer[ref Term]
        astbuf*: Buffer[AstNode]

proc newBuffer[T](): Buffer[T] =
    Buffer[T](
        impl: initTable[string, T]()
    )
proc `[]=`*[T](self: Buffer[T], uri: string, val: T) =
    self.impl[uri] = val
proc newBuffers*(): Buffers =
    Buffers(
        termbuf: newBuffer[ref Term](),
        astbuf: newBuffer[AstNode]()
    )
