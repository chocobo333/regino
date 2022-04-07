
import sets
import tables
import hashes
import sequtils

import il


proc literal*(_: typedesc[Value], lit: Literal): Value =
    Value(kind: ValueKind.Literal, litval: lit)
proc Bottom*(_: typedesc[Value]): Value =
    Value(kind: ValueKind.Bottom)
proc Unit*(_: typedesc[Value]): Value =
    Value(kind: ValueKind.Unit)
proc Bool*(_: typedesc[Value]): Value =
    Value(kind: ValueKind.Bool)
proc Integer*(_: typedesc[Value], bits: uint = 0): Value =
    Value(kind: ValueKind.Integer, bits: bits)
proc Float*(_: typedesc[Value], bits: uint = 0): Value =
    Value(kind: ValueKind.Float, bits: bits)
proc Char*(_: typedesc[Value]): Value =
    Value(kind: ValueKind.Char)
proc String*(_: typedesc[Value]): Value =
    Value(kind: ValueKind.String)
proc Pair*(_: typedesc[Value], first, second: Value): Value =
    Value(kind: ValueKind.Pair, first: first, second: second)
proc Array*(_: typedesc[Value], base: Value): Value =
    Value(kind: ValueKind.Array, base: base)
proc Array*(_: typedesc[Value], vals: seq[Value]): Value =
    Value(kind: ValueKind.Array, vals: vals)
proc Record*(_: typedesc[Value], members: Table[Ident, Value]): Value =
    Value(kind: ValueKind.Record, members: members)
proc Ptr*(_: typedesc[Value], pointee: Value): Value =
    Value(kind: ValueKind.Ptr, pointee: pointee)
proc Pi*(_: typedesc[Value], implicit: seq[GenericType], params: seq[Value], rety: Value): Value =
    Value(kind: ValueKind.Pi, implicit: implicit, params: params, rety: rety)
proc Arrow*(_: typedesc[Value], params: seq[Value], rety: Value): Value =
    Value(kind: ValueKind.Pi, implicit: @[], params: params, rety: rety)
proc Sum*(_: typedesc[Value], cons: Table[Ident, Value]): Value =
    Value(kind: ValueKind.Sum, cons: cons)
proc trait*(_: typedesc[Value], paty: (Pattern, Value), iss: seq[(Pattern, Value)], fns: seq[Function]): Value =
    Value(kind: ValueKind.Trait, paty: paty, iss: iss, fns: fns)
proc Singleton*(_: typedesc[Value], base: Value): Value =
    Value(kind: ValueKind.Singleton, base: base)
proc Distinct*(_: typedesc[Value], base: Value): Value =
    Value(kind: ValueKind.Distinct, base: base)
proc Intersection*(_: typedesc[Value], types: HashSet[Value]): Value =
    case types.len
    of 0:
        # assert false, ""
        Value.Unit
    of 1:
        toSeq(types.items)[0]
    else:
        Value(kind: ValueKind.Intersection, types: types)
proc Intersection*(_: typedesc[Value], types: seq[Value]): Value =
    Value.Intersection(types.toHashSet)
proc Union*(_: typedesc[Value], types: seq[Value]): Value =
    Value(kind: ValueKind.Union, types: types.toHashSet)
proc Union*(_: typedesc[Value], types: HashSet[Value]): Value =
    Value(kind: ValueKind.Union, types: types)
proc Var*(_: typedesc[Value], tv: TypeVar): Value =
    Value(kind: ValueKind.Var, tv: tv)
proc Gen*(_: typedesc[Value], gen: GenericType): Value =
    Value(kind: ValueKind.Gen, gt: gen)
proc Link*(_: typedesc[Value], to: Value): Value =
    if to.kind == ValueKind.Link:
        Value.Link(to.to)
    else:
        Value(kind: ValueKind.Link, to: to)
proc Univ*(_: typedesc[Value], level: uint = 0): Value =
    Value.literal(Literal(kind: LiteralKind.Univ, level: level))

proc dual*(self: Value): Value =
    case self.kind
    of ValueKind.Intersection:
        Value.Union(self.types)
    of ValueKind.Union:
        Value.Intersection(self.types)
    else:
        self

proc hash*(self: Value): Hash =
    result = self.kind.int
    result = result !& (
        case self.kind
        of ValueKind.Literal:
            0
        of ValueKind.Bottom:
            0
        of ValueKind.Unit:
            0
        of ValueKind.Bool:
            0
        of ValueKind.Integer:
            0
        of ValueKind.Float:
            0
        of ValueKind.Char:
            0
        of ValueKind.String:
            0
        of ValueKind.Pair:
            0
        of ValueKind.Array:
            0
        of ValueKind.ArrayV:
            0
        of ValueKind.Record:
            0
        of ValueKind.Ptr:
            0
        of ValueKind.Pi:
            0
        of ValueKind.Sum:
            0
        of ValueKind.Trait:
            0
        of ValueKind.Singleton:
            0
        of ValueKind.Distinct:
            0
        of ValueKind.Intersection:
            0
        of ValueKind.Union:
            0
        of ValueKind.Var:
            0
        of ValueKind.Gen:
            0
        of ValueKind.Link:
            0
    )
    result = !$result

proc compilable*(self: Value): bool =
    case self.kind
    of ValueKind.Literal:
        self.litval.kind != LiteralKind.Univ
    of ValueKind.Bottom:
        false
    of ValueKind.Unit:
        true
    of ValueKind.Bool..ValueKind.String:
        true
    of ValueKind.Ptr:
        self.pointee.compilable
    of ValueKind.Pair:
        self.first.compilable and self.second.compilable
    of ValueKind.Array:
        self.base.compilable
    of ValueKind.ArrayV:
        self.vals.all(compilable)
    of ValueKind.Record:
        toSeq(self.members.values).all(compilable)
    of ValueKind.Pi:
        self.params.all(compilable) and self.rety.compilable
    of ValueKind.Sum:
        false
    of ValueKind.Trait:
        false
    # of ValueKind.Sigma:
    #     self.first.compilable and self.second.compilable
    of ValueKind.Intersection, ValueKind.Union:
        false
    of ValueKind.Distinct:
        self.base.compilable
    of ValueKind.Link:
        self.to.compilable
    of ValueKind.Var:
        assert false
        false
    of ValueKind.Singleton:
        false
    of ValueKind.Gen:
        assert false
        false

proc typ*(self: Literal): Value =
    case self.kind
    of LiteralKind.unit:
        Value.Unit
    of LiteralKind.bool:
        Value.Bool
    of LiteralKind.integer:
        Value.Integer(self.intbits)
    of LiteralKind.float:
        Value.Float(self.floatbits)
    of LiteralKind.char:
        Value.Char
    of LiteralKind.string:
        Value.String
    of LiteralKind.Univ:
        Value.Univ(self.level+1)
proc isUniv*(self: Value): bool =
    self.kind == ValueKind.Literal and self.litval.kind == LiteralKind.Univ
proc typ*(self: Value): Value =
    case self.kind
    of ValueKind.Literal:
        self.litval.typ
    of ValueKind.Bottom:
        Value.Univ
    of ValueKind.Unit:
        Value.Univ
    of ValueKind.Bool:
        Value.Univ
    of ValueKind.Integer:
        Value.Univ
    of ValueKind.Float:
        Value.Univ
    of ValueKind.Char:
        Value.Univ
    of ValueKind.String:
        Value.Univ
    of ValueKind.Pair:
        let
            first = self.first.typ
            second = self.second.typ
        if first.isUniv and second.isUniv:
            Value.Univ(max(first.litval.level, second.litval.level))
        else:
            Value.Pair(first, second)
    of ValueKind.Array:
        let t = self.base.typ
        if t.isUniv:
            t
        else:
            Value.Array(t)
    of ValueKind.ArrayV:
        Value.Array(self.vals[0].typ)
    of ValueKind.Record:
        Value.Univ
    of ValueKind.Ptr:
        self.pointee.typ
    of ValueKind.Pi:
        Value.Univ
    of ValueKind.Sum:
        Value.Univ
    of ValueKind.Trait:
        Value.Univ
    of ValueKind.Singleton:
        self.base.typ
    of ValueKind.Distinct:
        self.base.typ
    of ValueKind.Intersection:
        Value.Univ
    of ValueKind.Union:
        Value.Univ
    of ValueKind.Var:
        Value.Univ
    of ValueKind.Gen:
        Value.Univ
    of ValueKind.Link:
        self.to.typ

import literals
proc `==`*(t1, t2: Value): bool =
    if t1.kind == t2.kind:
        case t1.kind
        of ValueKind.Literal:
            t1.litval == t2.litval
        of ValueKind.Bottom:
            true
        of ValueKind.Unit:
            true
        of ValueKind.Bool:
            true
        of ValueKind.Integer:
            t1.bits == t2.bits
        of ValueKind.Float:
            t1.bits == t2.bits
        of ValueKind.Char:
            true
        of ValueKind.String:
            true
        of ValueKind.Pair:
            t1.first == t2.first and t1.second == t2.second
        of ValueKind.Array:
            t1.base == t2.base
        of ValueKind.ArrayV:
            true
        of ValueKind.Record:
            true
        of ValueKind.Ptr:
            t1.pointee == t2.pointee
        of ValueKind.Pi:
            t1.implicit == t2.implicit and
            t1.params == t2.params and
            t1.rety == t2.rety
        of ValueKind.Sum:
            true
        of ValueKind.Trait:
            true
        of ValueKind.Singleton:
            t1.base == t2.base
        of ValueKind.Distinct:
            t1.base == t2.base
        of ValueKind.Intersection:
            t1.types == t2.types
        of ValueKind.Union:
            t1.types == t2.types
        of ValueKind.Var:
            t1.tv.id == t2.tv.id
        of ValueKind.Gen:
            t1.gt == t2.gt
        of ValueKind.Link:
            t1.to == t2.to
    else:
        false

when isMainModule:
    assert Value.Integer == Value.Integer
