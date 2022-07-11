
import strformat
import strutils
import sequtils

import ir


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
proc `$`*(self: Expression): string =
    # TODO:
    case self.kind
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
