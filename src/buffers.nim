
import tables
import il
import ast
import codegen


type
    Buffer[T] = Table[string, T]
    Buffers* = object
        termbuf*: Buffer[Term]
        astbuf*: Buffer[AstNode]
        modbuf*: Buffer[Module]
        tokenTypes*: seq[string]
        tokenModifiers*: seq[string]

proc newBuffer[T](): Buffer[T] =
    initTable[string, T]()

proc newBuffers*(): Buffers =
    Buffers(
        termbuf: newBuffer[Term](),
        astbuf: newBuffer[AstNode](),
        modbuf: newBuffer[Module]()
    )
