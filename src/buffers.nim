
import tables
import il
import ast


type
    Buffer[T] = Table[string, T]
    Buffers* = object
        termbuf*: Buffer[Term]
        astbuf*: Buffer[AstNode]

proc newBuffer[T](): Buffer[T] =
    initTable[string, T]()

proc newBuffers*(): Buffers =
    Buffers(
        termbuf: newBuffer[Term](),
        astbuf: newBuffer[AstNode]()
    )
