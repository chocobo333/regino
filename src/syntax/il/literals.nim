
import il


proc unit*(_: typedesc[Literal]): Literal =
    Literal(kind: LiteralKind.unit)
proc boolean*(_: typedesc[Literal], val: bool): Literal =
    Literal(kind: LiteralKind.bool, boolval: val)
proc integer*(_: typedesc[Literal], val: BiggestInt, bits: uint = 0): Literal =
    Literal(kind: LiteralKind.integer, intval: val, intbits: bits)
proc floating*(_: typedesc[Literal], val: BiggestFloat, bits: uint = 0): Literal =
    Literal(kind: LiteralKind.float, floatval: val, floatbits: bits)
proc char*(_: typedesc[Literal], val: char): Literal =
    Literal(kind: LiteralKind.char, charval: val)
proc str*(_: typedesc[Literal], val: string): Literal =
    Literal(kind: LiteralKind.string, strval: val)
proc Univ*(_: typedesc[Literal], level: uint): Literal =
    Literal(kind: LiteralKind.Univ, level: level)

converter toLiteral*(self: bool): Literal =
    Literal.boolean(self)
converter toLiteral*(self: int): Literal =
    Literal.integer(self)
converter toLiteral*(self: float): Literal =
    Literal.floating(self)
converter toLiteral*(self: char): Literal =
    Literal.char(self)
converter toLiteral*(self: string): Literal =
    Literal.str(self)

proc `==`*(a, b: Literal): bool =
    if a.kind == b.kind:
        case a.kind
        of LiteralKind.unit:
            true
        of LiteralKind.bool:
            a.boolval == b.boolval
        of LiteralKind.float:
            a.floatval == b.floatval and a.floatbits == b.floatbits
        of LiteralKind.integer:
            a.intbits == b.intbits and a.intval == b.intval
        of LiteralKind.char:
            a.charval == b.charval
        of LiteralKind.string:
            a.strval == b.strval
        of LiteralKind.Univ:
            a.level == b.level
    else:
        false
