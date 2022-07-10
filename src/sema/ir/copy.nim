
import tables
import sequtils

import ir
import constructors

proc copy*(self: PiType): PiType
proc copy*(self: Value): Value
proc copy*(self: Pattern): Pattern
proc copy*(self: GenericType): GenericType
proc copy*(self: FunctionSignature): FunctionSignature
proc copy*(self: Function): Function

proc copy*[A, B](self: (A, B)): (A, B) =
    let (a, b) = self
    (a.copy, b.copy)

proc copy*(self: Type): Type =
    case self.kind
    of TypeKind.Bottom:
        Type.Bottom
    of TypeKind.Unit:
        Type.Unit
    of TypeKind.Univ:
        Type.Univ(self.level)
    of TypeKind.Value:
        Type.value(self.val.copy)
    of TypeKind.Bool:
        Type.Bool
    of TypeKind.Integer:
        Type.Integer(self.nbits)
    of TypeKind.Float:
        Type.Float(self.nbits)
    of TypeKind.Char:
        Type.Char
    of TypeKind.CString:
        Type.CString
    of TypeKind.Pair:
        Type.Pair(self.first.copy, self.second.copy)
    of TypeKind.Record:
        var m = initTable[string, Type]()
        for k in self.members.keys:
            m[k] = self.members[k].copy
        Type.Record(m)
    of TypeKind.Object:
        var m = initTable[string, Type]()
        for k in self.members.keys:
            m[k] = self.members[k].copy
        Type.Object(m)
    of TypeKind.Array:
        Type.Array(self.length, self.base.copy)
    of TypeKind.Distinct:
        Type.Distinct(self.base.copy)
    of TypeKind.Singleton:
        Type.Singleton(self.base.copy)
    of TypeKind.Ptr:
        Type.Ptr(self.pointee.copy)
    of TypeKind.Arrow:
        Type.Arrow(self.params.map(copy), self.rety.copy)
    of TypeKind.Cons:
        Type.Cons(self.cons.copy, self.args.map(copy))
    of TypeKind.Recursive:
        Type.Recursive(self.self, self.body.copy)
    of TypeKind.Trait:
        Type.Trait(self.paty.copy, self.iss.mapIt(it.copy), self.fns.mapIt(it.copy), self.fnss.mapIt(it.copy))
    of TypeKind.Var:
        Type.Link(self)
    of TypeKind.Gen:
        Type.Gen(self.gt.copy)
    of TypeKind.Link:
        Type.Link(self.to.copy)

proc copy*(self: PiType): PiType =
    # TODO:
    self

proc copy*(self: Pattern): Pattern =
    # TODO:
    self
    # case self.kind
    # of PatternKind.Literal:
    #     Pattern.Literal
    # of PatternKind.Ident:
    #     Pattern.Ident
    # of PatternKind.Tuple:
    #     Pattern.Tuple
    # of PatternKind.Record:
    #     Pattern.Record
proc copy*(self: Value): Value = 
    case self.kind:
    of ValueKind.Unit:
        Value.Unit
    of ValueKind.Univ:
        Value.Univ(self.level)
    of ValueKind.Bool:
        Value.Bool(self.boolval)
    of ValueKind.Integer:
        Value.Integer(self.intval, self.intbits)
    of ValueKind.Float:
        Value.Float(self.floatval, self.floatbits)
    of ValueKind.Char:
        Value.Char(self.charval)
    of ValueKind.CString:
        Value.CString(self.strval)
    of ValueKind.Array:
        Value.Array(self.vals.map(copy))
    of ValueKind.Function:
        Value.Function(self.fn.copy)

proc copy*(self: GenericType): GenericType =
    GenericType(id: self.id, ub: self.ub.copy, typ: self.typ.copy)

proc copy*(self: FunctionSignature): FunctionSignature =
    # TODO:
    self
proc copy*(self: Function): Function =
    # TODO:
    self