
import hashes
import tables

import ir

proc hash*(self: Ident): Hash =
    self.name.hash
proc hash*(self: Function): Hash =
    # TODO:
    result = 0
    result = !$ result
proc hash*(self: FunctionSignature): Hash =
    # TODO:
    result = 0
    result = !$ result
proc hash*(self: Pattern): Hash =
    # TODO:
    result = 0
    result = !$ result
proc hash*(self: Value): Hash =
    # TODO:
    result = 0
    result = !$ result
proc hash*(self: TypeVar): Hash =
    self.id.hash
proc hash*(self: GenericType): Hash =
    self.id.hash
proc hash*(self: Type): Hash =
    result = 0
    result = result !& self.kind.int
    result = result !& (case self.kind
    of TypeKind.Bottom, TypeKind.Unit, TypeKind.Bool, TypeKind.Char, TypeKind.CString:
        0
    of TypeKind.Univ:
        self.level.hash
    of TypeKind.Value:
        self.val.hash
    of TypeKind.Integer, TypeKind.Float:
        self.nbits.hash
    of TypeKind.Pair:
        self.first.hash !& self.second.hash
    of TypeKind.Array:
        self.base.hash !& self.length.hash
    of TypeKind.Record:
        var val = 0
        for (k, v) in self.members.pairs:
            val = val !& k.hash !& v.hash
        val
    of TypeKind.Object:
        var val = 0
        for (k, v) in self.members.pairs:
            val = val !& k.hash !& v.hash
        val
    of TypeKind.Arrow:
        self.params.hash !& self.rety.hash
    of TypeKind.Cons:
        self.cons.hash !& self.args.hash
    of TypeKind.Distinct:
        self.base.hash
    of TypeKind.Singleton:
        self.base.hash
    of TypeKind.Ptr:
        self.pointee.hash
    of TypeKind.Recursive:
        self.self.hash !& self.body.hash
    of TypeKind.Trait:
        self.paty.hash !& self.iss.hash !& self.fns.hash !& self.fnss.hash
    of TypeKind.Var:
        self.tv.hash
    of TypeKind.Gen:
        self.gt.hash
    of TypeKind.Link:
        self.to.hash
    )
    result = !$ result
