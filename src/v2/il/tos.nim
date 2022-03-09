
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
import nre
proc `$`*(self: Ident): string =
    let
        re = re"(*UTF8)^[_\p{L}\p{Nl}ー][_\p{L}\p{N}ー]*$"
    if self.name.match(re).isSome:
        self.name
    else:
        fmt"`{self.name}`"
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
    of ExpressionKind.Tuple:
        let s = self.exprs.join(", ")
        fmt"({s})"
    of ExpressionKind.Seq:
        let s = self.exprs.join(", ")
        fmt"[{s}]"
    of ExpressionKind.Record:
        let members = self.members.mapIt(fmt"{it[0]}: {it[1]}").join(", ")
        fmt"({members})"
    of ExpressionKind.If:
        let
            elifs = self.elifs.map(`$`).join("\n")[2..^1]
            elseb = if self.elseb.isSome:
                &"\nelse\n{self.elseb.get}"
            else:
                ""
        &"{elifs}{elseb}"
    of ExpressionKind.Case:
        ""
    of ExpressionKind.Call:
        let args = self.args.join(", ")
        fmt"{self.callee}({args})"
    of ExpressionKind.Command:
        let args = self.args.join(", ")
        fmt"{self.callee} {args}"
    of ExpressionKind.Dot:
        fmt"{self.lhs}.{self.rhs}"
    of ExpressionKind.Binary:
        fmt"{self.lhs} {self.op} {self.rhs}"
    of ExpressionKind.Prefix:
        fmt"{self.op}{self.expression}"
    of ExpressionKind.Postfix:
        fmt"{self.expression}{self.op}"
    of ExpressionKind.Bracket:
        let args = self.args.join(", ")
        fmt"{self.callee}[{args}]"
    of ExpressionKind.Malloc:
        fmt"malloc({self.mtype}, {self.msize})"
    of ExpressionKind.Typeof:
        fmt"typeof({self.`typeof`})"
    of ExpressionKind.Fail:
        fmt"failed term"
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
    of PatternKind.Tuple:
        let s = self.patterns.map(`$`).join(", ")
        fmt"({s})"
    of PatternKind.Record:
        let members = self.members.mapIt(fmt"{it[0]}: {it[1]}").join(", ")
        fmt"({members})"
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
        implicit = block:
            let imp = self.implicit.map(`$`).join(", ")
            if self.implicit.len == 0:
                ""
            else:
                fmt"[{imp}]"
        params = self.params.map(`$`).join(", ")
        rety = if self.rety.isNone: "" else: fmt" -> {self.rety.get}"
    fmt"{implicit}({params}){rety}"
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
    of StatementKind.LetSection:
        let iddefs = self.iddefs.map(`$`).join("\n").indent(2)
        &"let\n{iddefs}"
    of StatementKind.VarSection:
        let iddefs = self.iddefs.map(`$`).join("\n").indent(2)
        &"var\n{iddefs}"
    of StatementKind.ConstSection:
        let iddefs = self.iddefs.map(`$`).join("\n").indent(2)
        &"const\n{iddefs}"
    of StatementKind.TypeSection:
        let iddefs = self.iddefs.map(`$`).join("\n").indent(2)
        &"type\n{iddefs}"
    of StatementKind.Asign:
        fmt"{self.pat} = {self.val}"
    of StatementKind.Funcdef:
        $self.fn
    of StatementKind.Meta:
        $self.meta
    of StatementKind.Discard:
        if self.`discard`.isSome:
            fmt"discard {self.`discard`.get}"
        else:
            fmt"discard"
    of StatementKind.Comments:
        "#" & self.comments.join("\n#")
    of StatementKind.Expression:
        $self.expression
    of StatementKind.Fail:
        "failed term"
proc `$`*(self: Program): string =
    self.map(`$`).join("\n")

when isMainModule:
    import stmts
    import exprs
    import literals
    import idents
    import suites
    import branches
    let
        a = Expression.Id("a")
        b = Expression.literal(3)
        c = Expression.Dot(a, b)
        d = Expression.Bracket(c, [a, b])
        e = Expression.If([newElif(a, @[Statement a])], some newSuite([Statement a]))
    echo c
    echo d
    echo e
