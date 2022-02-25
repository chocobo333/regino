
import il


proc unit*(_: typedesc[Literal]): Literal =
    Literal(kind: LiteralKind.unit)
proc bool*(_: typedesc[Literal], val: bool): Literal =
    Literal(kind: LiteralKind.bool, boolval: val)
proc integer*(_: typedesc[Literal], val: BiggestInt, bits: uint = 0): Literal =
    Literal(kind: LiteralKind.integer, intval: val, intbits: bits)
proc float*(_: typedesc[Literal], val: BiggestFloat, bits: uint = 0): Literal =
    Literal(kind: LiteralKind.float, floatval: val, floatbits: bits)
proc char*(_: typedesc[Literal], val: char): Literal =
    Literal(kind: LiteralKind.char, charval: val)
proc string*(_: typedesc[Literal], val: string): Literal =
    Literal(kind: LiteralKind.string, strval: val)
proc Univ*(_: typedesc[Literal], level: uint): Literal =
    Literal(kind: LiteralKind.Univ, level: level)
