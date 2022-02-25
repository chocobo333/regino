
import strformat
import strutils
import sequtils
import options

import il


proc `$`*(self: Statement): string
proc `$`*(self: Expression): string
proc `$`*(self: Literal): string =
    case self.kind
    of LiteralKind.unit:
        "()"
    of LiteralKind.bool:
        $self.boolval
    of LiteralKind.integer:
        if self.intbits == 0:
            $self.intval
        else:
            fmt"{self.intval}'i{self.intbits}"
    of LiteralKind.float:
        if self.floatbits == 0:
            $self.floatval
        else:
            fmt"{self.floatval}'f{self.floatbits}"
    of LiteralKind.char:
        fmt"'{self.charval}'"
    of LiteralKind.string:
        self.strval.escape
    of LiteralKind.Univ:
        fmt"Type{self.level}"
proc `$`*(self: Ident): string =
    self.name
proc `$`*(self: Suite): string =
    self.stmts.map(`$`).join("\n").indent(2)
proc `$`*(self: ElifBranch): string =
    let suite = $self.suite
    &"elif {self.cond}:\n{suite}"
proc `$`*(self: Expression): string =
    case self.kind
    of ExpressionKind.Literal:
        $self.litval
    of ExpressionKind.Ident:
        $self.ident
    of ExpressionKind.Pair:
        fmt"({self.first}, {self.second})"
    of ExpressionKind.Record:
        let members = self.members.mapIt(fmt"{it[0]}: {it[1]}").join(", ")
        fmt"({members})"
    of ExpressionKind.If:
        let
            elifs = self.elifs.map(`$`).join("\n")[2..^1]
            elseb = $self.elseb
        &"{elifs}\nelse\n{elseb}"
    of ExpressionKind.Case:
        ""
    of ExpressionKind.Apply:
        let args = self.args.join(", ")
        fmt"{self.callee}({args})"
    of ExpressionKind.Dot:
        fmt"{self.lhs}.{self.rhs}"
    of ExpressionKind.Bracket:
        let args = self.args.join(", ")
        fmt"{self.callee}[{args}]"
    of ExpressionKind.Malloc:
        fmt"malloc({self.mtype}, {self.msize})"
proc `$`*(self: Metadata): string =
    let params =
        if self.params.len == 0:
            ""
        else:
            let params = self.params.join("\n")
            fmt": {params}"
    case self.kind
    of MetadataKind.Link:
        fmt"![link{params}]"
    of MetadataKind.ImportLL:
        fmt"![importll{params}]"
    of MetadataKind.Subtype:
        fmt"![subtype{params}]"
    of MetadataKind.Userdef:
        fmt"![{self.name}{params}]"
proc `$`*(self: Pattern): string =
    case self.kind
    of PatternKind.Literal:
        $self.litval
    of PatternKind.Ident:
        self.ident.name
    of PatternKind.Dot:
        fmt"{self.lhs}.{self.rhs}"
    of PatternKind.Bracket:
        let args = self.args.join(", ")
        fmt"{self.callee}[{args}]"
    of PatternKind.Pair:
        fmt"({self.first}, {self.second})"
    of PatternKind.UnderScore:
        "_"
proc `$`*(self: IdentDef): string =
    let
        pat = $self.pat
        typ = if self.typ.isNone: "" else: fmt": {self.typ.get}"
        default = if self.default.isNone: "" else: fmt" = {self.default.get}"
    pat & typ & default
proc `$`*(self: FunctionParam): string =
    let
        implicit = self.implicit.map(`$`).join(", ")
        params = self.params.map(`$`).join(", ")
        rety = if self.rety.isNone: "" else: fmt" -> {self.rety.get}"
    fmt"[implicit](params){rety}"
proc `$`*(self: Function): string =
    let
        metadata = if self.metadata.isNone: "" else: fmt" {self.metadata.get}"
        suite = if self.suite.isNone: "" else: fmt"{self.suite.get}"
    &"func {self.id}{self.param}{metadata}:\n{suite}"
proc `$`*(self: Statement): string =
    case self.kind
    of StatementKind.Block:
        if self.label.isNone:
            &"block\n{self.`block`}"
        else:
            &"block {self.label.get}\n{self.`block`}"
    of StatementKind.For:
        &"for {self.pat} in {self.val}:\n{self.suite}"
    of StatementKind.While:
        "while" & ($self.branch)[4..^1]
    of StatementKind.Loop:
        if self.label.isNone:
            &"loop\n{self.`block`}"
        else:
            &"loop {self.label.get}\n{self.`block`}"
    of StatementKind.Let:
        fmt"let {self.iddef}"
    of StatementKind.Var:
        fmt"var {self.iddef}"
    of StatementKind.Const:
        fmt"const {self.iddef}"
    of StatementKind.Typedef:
        let iddefs = self.iddefs.map(`$`).join("\n").indent(2)
        &"type\n{iddefs}"
    of StatementKind.Asign:
        fmt"{self.pat} = {self.val}"
    of StatementKind.Funcdef:
        ""
    of StatementKind.Meta:
        $self.meta
    of StatementKind.Expression:
        $self.expression
proc `$`*(self: Program): string =
    "program"

when isMainModule:
    import stmts
    import exprs
    import literals
    import idents
    import suites
    import branches
    let
        a = Expression.Ident("a")
        b = Expression.literal(3)
        c = Expression.Dot(a, b)
        d = Expression.Bracket(c, [a, b])
        e = Expression.If([newElif(a, @[Statement a])], newSuite([Statement a]))
    echo c
    echo d
    echo e
