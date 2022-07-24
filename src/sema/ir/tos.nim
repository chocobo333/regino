
import strformat
import strutils
import sequtils
import tables
import options

import ir


proc `$`*(self: Ident): string =
    self.name
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
        $self.kind
    of TypeKind.Record:
        $self.kind
    of TypeKind.Object:
        $self.kind
    of TypeKind.Arrow:
        $self.kind
    of TypeKind.Cons:
        $self.kind
    of TypeKind.Distinct:
        "distinct {self.base}"
    of TypeKind.Singleton:
        fmt"singleton {self.base}"
    of TypeKind.Ptr:
        fmt"ptr {self.pointee}"
    of TypeKind.Recursive:
        $self.kind
    of TypeKind.Trait:
        $self.kind
    of TypeKind.Var:
        fmt"'{self.id}"
    of TypeKind.Select:
        $self.kind
    of TypeKind.RecursiveVar:
        $self.kind
    of TypeKind.Intersection:
        $self.kind
    of TypeKind.Union:
        $self.kind
    of TypeKind.Gen:
        $self.kind
    of TypeKind.Link:
        fmt"~{self.to}"
proc `$`*(self: PiType): string =
    if self.ident.isSome:
        $self.ident.get
    else:
        if self.params.len == 0:
            $self.rety
        else:
            fmt"{self.params}{self.rety}"

proc `$`*(self: Literal): string =
    case self.kind
    of LiteralKind.Unit:
        "()"
    of LiteralKind.Univ:
        fmt"Type{self.level}"
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

proc `$`*(self: Expression): string
proc `$`*(self: TypeExpression): string =
    # TODO: unit
    if self.kind == TypeExpressionKind.Expression:
        $self.expression
    else:
        $self.kind
proc `$`*(self: Pattern): string =
    case self.kind
    of PatternKind.Literal:
        "Pattern"
    of PatternKind.Ident:
        fmt"{self.ident}: {self.typ}"
    of PatternKind.Tuple:
        "Pattern"
    of PatternKind.Record:
        "Pattern"
proc `$`*(self: IdentDef): string =
    let
        typ = self.typ.map(`$`).get("")
        default = self.default.map(`$`).get("")
    &"{self.pat}\n{typ}\n{default}"
proc `$`*(self: TypeDef): string =
    # TODO: unit
    &"{self.ident.name}\n{self.typ}"
proc `$`*(self: Expression): string =
    template kind2str(self: Expression): string =
        if self.typ.isNil:
            $self.kind
        else:
            fmt"{self.kind}: {self.typ}"
    # TODO: unit
    result = case self.kind
    of ExpressionKind.Literal:
        "Lit " & $self.litval
    of ExpressionKind.Ident:
        fmt"{self.ident}: {self.typ}"
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
        let a = self.iddefs.map(`$`).join("\n").indent(2)
        &"{self.kind2str}\n{a}"
    of ExpressionKind.VarSection:
        ""
    of ExpressionKind.ConsSection:
        ""
    of ExpressionKind.TypeSection:
        self.kind2str & "\n" & ($self.typedef).indent(2)
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
        self.kind2str & "\n" & self.expressions.map(`$`).join("\n").indent(2)
    of ExpressionKind.Typeof:
        self.kind2str & "\n" & ($self.`typeof`).indent(2)
    of ExpressionKind.Malloc:
        ""
    of ExpressionKind.Realloc:
        ""
    of ExpressionKind.PtrSet:
        ""
    of ExpressionKind.PtrGet:
        ""

proc `$`*(self: Symbol): string =
    let
        kind = $self.kind
        id = self.ident.name
        loc = self.ident.loc
        typ = $self.typ
    fmt"{loc}: ({kind}){id}: {typ}"
proc `$`*(self: Scope): string =
    @[$self.vars, $self.types, $self.funcs].join("\n")
