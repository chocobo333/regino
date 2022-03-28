
import tables
import il
# import codegen


type
    Buffer[T] = Table[string, T]
    Buffers* = object
        termbuf*: Buffer[Program]
        astbuf*: Buffer[Program]
        # modbuf*: Buffer[Module]

proc newBuffer[T](): Buffer[T] =
    initTable[string, T]()

proc newBuffers*(): Buffers =
    Buffers(
        termbuf: newBuffer[Program](),
        astbuf: newBuffer[Program](),
        # modbuf: newBuffer[Module]()
    )
