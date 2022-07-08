
import tables
import sequtils

import ir
import constructors

proc copy*(self: Pattern): Pattern
proc copy*[A, B](self: (A, B)): (A, B) =
    let (a, b) = self
    (a.copy, b.copy)
proc copy*(self: Pattern): Pattern =
    # TODO:
    case self.kind
    of PatternKind.Literal:
        Pattern.Literal
    of PatternKind.Ident:
        Pattern.Ident
    of PatternKind.Tuple:
        Pattern.Tuple
    of PatternKind.Record:
        Pattern.Record
proc copy*(self: Type): Type =
    case self.kind
    of TypeKind.Bottom:
        Type.Bottom
    of TypeKind.Unit:
        Type.Unit
    of TypeKind.Univ:
        Type.Univ(self.level)
    of TypeKind.Value:
        Type.Value(self.val)
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
    of TypeKind.Arrow:
        Type.Arrow(self.params.map(copy), self.rety)
    of TypeKind.Cons:
        Type.Cons(self.cons, self.args.map(copy))
    of TypeKind.Recursive:
        Type.Recursive(self.self, self.body.copy)
    of TypeKind.Trait:
        Type.Trait(self.paty.copy, self.iss.mapIt(it.copy), self.fns.mapIt(it.copy), self.fnss.mapIt(it.copy))
    of TypeKind.Var:
        Type.Link(self)
    of TypeKind.Gen:
        Type.Gen(self.gt)
    of TypeKind.Link:
        Type.Link(self.to.copy)
