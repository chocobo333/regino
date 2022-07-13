
import strformat
import strutils
import sequtils

import ir


proc `$`*(self: Type): string =
    # TODO:
    case self.kind
    of TypeKind.Bottom:
        "⊥"
    of TypeKind.Unit:
        "()"
    of TypeKind.Univ:
        fmt"U{self.level}"
    of TypeKind.Value:
        fmt"<{self.val}>"
    of TypeKind.Bool:
        "bool"
    of TypeKind.Integer:
        fmt"i{self.nbits}"
    of TypeKind.Float:
        fmt"f{self.nbits}"
    of TypeKind.Char:
        "char"
    of TypeKind.CString:
        "cstring"
    of TypeKind.Pair:
        fmt"({self.first}, {self.second})"
    of TypeKind.Array:
        ""
    of TypeKind.Record:
        ""
    of TypeKind.Object:
        ""
    of TypeKind.Arrow:
        ""
    of TypeKind.Cons:
        ""
    of TypeKind.Distinct:
        "distinct {self.base}"
    of TypeKind.Singleton:
        fmt"singleton {self.base}"
    of TypeKind.Ptr:
        fmt"ptr {self.pointee}"
    of TypeKind.Recursive:
        ""
    of TypeKind.Trait:
        ""
    of TypeKind.Var:
        ""
    of TypeKind.Gen:
        ""
    of TypeKind.Link:
        fmt"~{self.to}"
proc `$`*(self: Literal): string =
    case self.kind
    of LiteralKind.Unit:
        "()"
    of LiteralKind.Bool:
        $self.boolval
    of LiteralKind.Integer:
        case self.intbits
        of 0:
            $self.intval
        else:
            fmt"{self.intval}'{self.intbits}"
    of LiteralKind.Float:
        case self.floatbits
        of 0:
            $self.floatval
        else:
            fmt"{self.floatval}'{self.floatbits}"
    of LiteralKind.Char:
        fmt"'{self.charval}'"
    of LiteralKind.CString:
        self.strval.escape
    of LiteralKind.Univ:
        fmt"Type{self.level}"
proc `$`*(self: Expression): string =
    # TODO:
    result = case self.kind
    of ExpressionKind.Literal:
        $self.litval
    of ExpressionKind.Ident:
        self.ident.name
    of ExpressionKind.Call:
        let
            args = self.args.map(`$`).join(", ")
        fmt"{self.callee}({args})"
    of ExpressionKind.Apply:
        let
            args = self.args.map(`$`).join(", ")
        fmt"{self.callee}[{args}]"
    of ExpressionKind.If:
        &"if {self.cond}:\n{self.then}\nelse:\n{self.els}"
    of ExpressionKind.Case:
        ""
    of ExpressionKind.Pair:
        ""
    of ExpressionKind.Array:
        ""
    of ExpressionKind.Record:
        ""
    of ExpressionKind.ObjCons:
        ""
    of ExpressionKind.Ref:
        ""
    of ExpressionKind.Import:
        ""
    of ExpressionKind.LetSection:
        ""
    of ExpressionKind.VarSection:
        ""
    of ExpressionKind.ConsSection:
        ""
    of ExpressionKind.TypeSection:
        ""
    of ExpressionKind.Assign:
        ""
    of ExpressionKind.Funcdef:
        ""
    of ExpressionKind.ImportLL:
        ""
    of ExpressionKind.Loop:
        ""
    of ExpressionKind.Discard:
        ""
    of ExpressionKind.Seq:
        ""
    of ExpressionKind.Typeof:
        ""
    of ExpressionKind.Malloc:
        ""
    of ExpressionKind.Realloc:
        ""
    of ExpressionKind.PtrSet:
        ""
    of ExpressionKind.PtrGet:
        ""
    if not self.typ.isNil:
        result = fmt"{result}: {self.typ}"
