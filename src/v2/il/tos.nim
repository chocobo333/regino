
import strformat
import strutils
import sequtils
import options
import tables
import sets
import sugar
import algorithm

import il
import ../lineinfos


proc `$`*(self: Statement): string
proc `$`*(self: Expression): string
proc `$`*(self: FunctionParam): string
proc `$`*(self: TypeExpression): string
proc `$`*(self: Pattern): string
proc `$`*(self: Suite): string
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
proc `$$`*(self: Ident): string =
    self.name
proc `$`*(self: Suite): string =
    self.stmts.map(`$`).join("\n").indent(2)
proc `$`*(self: ElifBranch): string =
    let suite = $self.suite
    &"elif {self.cond}:\n{suite}"
proc `$`*(self: OfBranch): string =
    let suite = $self.suite
    &"of {self.pat}:\n{suite}"
proc `$`*(self: Expression): string =
    case self.kind
    of ExpressionKind.Literal:
        $self.litval
    of ExpressionKind.Ident:
        $self.ident
    of ExpressionKind.Tuple:
        let s = self.exprs.join(", ")
        fmt"({s})"
    of ExpressionKind.Array:
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
    of ExpressionKind.When:
        let
            elifs = "when" & self.elifs.map(`$`).join("\n")[4..^1]
            elseb = if self.elseb.isSome:
                &"\nelse\n{self.elseb.get}"
            else:
                ""
        &"{elifs}{elseb}"
    of ExpressionKind.Case:
        let
            ofs = if self.ofs.len == 0: "" else: ("\n" & self.ofs.map(`$`).join("\n"))
            default = "\n" & self.default.map(it => "default:\n" & $it).get("")
        fmt"case {self.val}{ofs}{default}"
    of ExpressionKind.Call:
        let args = self.args.join(", ")
        fmt"{self.callee}({args})"
    of ExpressionKind.Command:
        let args = self.args.join(", ")
        fmt"{self.callee} {args}"
    of ExpressionKind.Dot:
        fmt"{self.lhs}.{self.rhs}"
    of ExpressionKind.Binary:
        fmt"{self.lhs} {$$self.op} {self.rhs}"
    of ExpressionKind.Prefix:
        fmt"{$self.op}{self.expression}"
    of ExpressionKind.Postfix:
        fmt"{self.expression}{$self.op}"
    of ExpressionKind.Block:
        if self.label.isNone:
            &"block:\n{self.`block`}"
        else:
            &"block: {self.label.get}\n{self.`block`}"
    of ExpressionKind.Bracket:
        let args = self.args.join(", ")
        fmt"{self.callee}[{args}]"
    of ExpressionKind.Lambda:
        let
            params = $self.param
            suite = $self.body
        if suite.count('\n') == 0:
            &"func{params}: {suite[2..^1]}"
        else:
            &"func{params}:\n{suite}"
    of ExpressionKind.Malloc:
        fmt"malloc({self.mtype}, {self.msize})"
    of ExpressionKind.Typeof:
        fmt"typeof({self.`typeof`})"
    of ExpressionKind.Ref:
        fmt"ref {self.`ref`}"
    of ExpressionKind.FnType:
        let args = self.args.join(", ")
        fmt"func({args}) -> {self.rety}"
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
        if self.index.isSome:
            fmt"{self.ident}[{self.index.get}]"
        else:
            $self.ident
    of PatternKind.Dot:
        fmt"{self.lhs}.{self.rhs}"
    # of PatternKind.Bracket:
    #     let args = self.args.join(", ")
    #     fmt"{self.callee}[{args}]"
    of PatternKind.Tuple:
        let
            tag = self.tag.map(`$`).get("")
            s = self.patterns.map(`$`).join(", ")
        fmt"{tag}({s})"
    of PatternKind.Record:
        let
            tag = self.tag.map(`$`).get("")
            members = self.members.mapIt(fmt"{it[0]}: {it[1]}").join(", ")
        fmt"{tag}({members})"
    of PatternKind.UnderScore:
        "_"
proc `$`*(self: IdentDef): string =
    let
        pat = $self.pat
        typ = if self.typ.isNone: "" else: fmt": {self.typ.get}"
        default = if self.default.isNone: "" else: fmt" = {self.default.get}"
    pat & typ & default
proc `$`*(self: GenTypeDef): string =
    let ub = if self.ub.isSome: fmt" <: {self.ub.get}" else: ""
    fmt"{self.id}{ub}"
proc `$`*(self: TypeDef): string =
    let
        id = $self.id
        params =
            if self.params.isNone:
                ""
            else:
                let params = self.params.get.map(`$`).join(", ")
                fmt"[{params}]"
        typ = $self.typ
    fmt"{id}{params} = {typ}"
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
        fn = ["func", "prop"][int self.isProp]
        metadata = if self.metadata.isNone: "" else: fmt" {self.metadata.get}"
        suite = if self.suite.isNone: "" else: fmt"{self.suite.get}"
    &"{fn} {self.id}{self.param}{metadata}:\n{suite}"
proc `$`*(self: Statement): string =
    case self.kind
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
        let typedefs = self.typedefs.map(`$`).join("\n").indent(2)
        &"type\n{typedefs}"
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
proc `$`*(self: SumConstructor): string =
    $self.id & (
        case self.kind
        of SumConstructorKind.NoField:
            ""
        of SumConstructorKind.UnnamedField:
            let types = self.types.join(", ")
            fmt"({types})"
        of SumConstructorKind.NamedField:
            proc `$`(field: (Ident, Expression)): string =
                let
                    (id, typ) = field
                fmt"{id}: {typ}"
            let fields = self.fields.map(`$`).join(", ")
            fmt"({fields})"
    )
proc `$`*(self: SumType): string =
    let cons = self.constructors.join("\n").indent(2)
    &"variant\n{cons}"
proc `$`*(self: Trait): string =
    case self.kind
    of TraitKind.Is:
        let
            val = $self.val
            pat = $self.pat
        fmt"{pat} is {val}"
    of TraitKind.Func:
        let fn = $self.fn
        fmt"{fn}"

proc `$`*(self: TraitType): string =
    let traits =
        if self.traits.len == 0:
            ""
        else:
            "\n" & self.traits.map(`$`).join("\n").indent(2)
    &"trait {self.pat}: {self.typ}{traits}"
proc `$`*(self: TypeExpression): string =
    result = if self.isRef:
        "ref "
    else:
        ""
    result &= (
        case self.kind
        of TypeExpressionKind.Object:
            let
                members = self.members.mapIt(fmt"{it[0]}: {it[1]}").join("\n").indent(2)
            &"object\n{members}"
        of TypeExpressionKind.Sum:
            $self.sum
        of TypeExpressionKind.Distinct:
            fmt"distinct {self.base}"
        of TypeExpressionKind.Trait:
            $self.trait
        of TypeExpressionKind.Expression:
            $self.expression
    )
proc `$`*(self: Program): string =
    self.stmts.map(`$`).join("\n")

proc `$`*(self: Value): string
proc `$`*(self: TypeVar): string =
    let
        c = toSeq('a'..'z')
        n = c.len
    var
        id = self.id - 1
    while id > n:
        let
            index = id mod n
        result.add c[index]
        id = id div n
    result.add c[id mod n]
    result.add "'"
    result.reverse
    result = fmt"{result}({self.lb}, {self.ub})"
proc `$`*(self: GenericType): string =
    self.ident.name
proc `$$`*(self: GenericType): string =
    let ub = if self.ub.kind != ValueKind.Unit: fmt" <: {self.ub}" else: ""
    fmt"{self.ident.name}{ub}"
proc `$`*(self: Value): string =
    case self.kind
    of ValueKind.Literal:
        $self.litval
    of ValueKind.Bottom:
        "Bottom"
    of ValueKind.Unit:
        "Unit"
    of ValueKind.Bool:
        "bool"
    of ValueKind.Integer:
        if self.bits == 0:
            "int"
        else:
            fmt"int'i{self.bits}"
    of ValueKind.Float:
        if self.bits == 0:
            "float"
        else:
            fmt"float'f{self.bits}"
    of ValueKind.Char:
        "char"
    of ValueKind.String:
        "string"
    of ValueKind.Pair:
        fmt"({self.first}, {self.second})"
    of ValueKind.Array:
        fmt"array[{self.base}]"
    of ValueKind.ArrayV:
        let vals = self.vals.join(", ")
        fmt"[{vals}]"
    of ValueKind.Record:
        var res = "("
        for (id, val) in self.members.pairs:
            res.add fmt"{id}: {val}, "
        res[0..^3] & ")"
    of ValueKind.Ptr:
        fmt"ptr {self.pointee}"
    of ValueKind.Pi:
        let
            imp =
                if self.implicit.len == 0:
                    ""
                else:
                    let imp = self.implicit.map(`$$`).join(", ")
                    fmt"[{imp}]"
            params =
                if self.params.len == 0:
                    ""
                else:
                    let params = self.params.join(", ")
                    fmt"({params})"
        fmt"{imp}{params} -> {self.rety}"
    of ValueKind.Sum:
        "Sum"
    of ValueKind.Trait:
        "Trait"
    of ValueKind.Singleton:
        fmt"sigleton[{self.base}]"
    of ValueKind.Distinct:
        fmt"distinct {self.base}"
    of ValueKind.Intersection:
        toSeq(self.types).join("^")
    of ValueKind.Union:
        toSeq(self.types).join"\/"
    of ValueKind.Cons:
        let
            imp = self.implicit.map(`$$`).join(", ")
        fmt"[{imp}]{self.rety}"
    of ValueKind.Var:
        $self.tv
    of ValueKind.Gen:
        $self.gt
    of ValueKind.Link:
        $self.to

proc `$`*(self: Symbol): string =
    let
        kind =
            if self.kind == SymbolKind.Typ:
                "Type"
            else:
                $self.kind
        id = if self.global:
            $self.id
        else:
            $self.id
        typ = $self.typ
        impl =
            case self.kind
            of SymbolKind.Func:
                "..."
            of SymbolKind.Var..SymbolKind.Param:
                $self.decl_iddef
            of SymbolKind.Typ:
                $self.decl_typedef
            of SymbolKind.GenParam:
                $self.decl_gendef
        loc = self.id.loc
    fmt"{loc}: ({kind}){id}: {typ} ({impl})"
proc `$`*(self: Scope): string =
    var
        tmp = self
        scopes: seq[Table[string, seq[Symbol]]]
        consts: seq[Table[string, seq[Symbol]]]
    while not tmp.isNil:
        scopes.add tmp.syms
        consts.add tmp.consts
        tmp = tmp.parent
    result = if scopes.foldl(a + b.len, 0) == 0:
        "{}"
    else:
        for scope in scopes:
            for (key, val) in scope.pairs:
                let
                    val =
                        if val.len == 1:
                            $val[0]
                        else:
                            val.map(`$`).join("\n").indent(2)
                result &= &"\"{key}\": {val},\n"
        &"{{\n{result[0..^3].indent(2)}\n}}"
    result.add "\n"
    result.add if consts.foldl(a + b.len, 0) == 0:
        "{}"
    else:
        var ret: string
        for scope in consts:
            for (key, val) in scope.pairs:
                ret &= &"\"{key}\" = {val},\n"
        &"{{\n{ret[0..^3].indent(2)}\n}}"

proc treeRepr*(self: Ident): string =
    "Ident\n" & ($self).indent(2)
proc treeRepr*(self: Expression): string =
    case self.kind
    of ExpressionKind.Literal:
        &"Lit\n  {self.litval}"
    of ExpressionKind.Ident:
        self.ident.treeRepr
    of ExpressionKind.Tuple:
        let s = self.exprs.join(", ")
        fmt"({s})"
    of ExpressionKind.Array:
        "Array\n" & self.exprs.map(treeRepr).join("\n").indent(2)
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
    of ExpressionKind.When:
        let
            elifs = "when" & self.elifs.map(`$`).join("\n")[4..^1]
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
        "Command\n" & (@[self.callee] & self.args).map(treeRepr).join("\n").indent(2)
    of ExpressionKind.Dot:
        fmt"{self.lhs}.{self.rhs}"
    of ExpressionKind.Binary:
        "Infix\n" & (&"{self.op.treeRepr}\n{self.lhs.treeRepr}\n{self.rhs.treeRepr}").indent(2)
    of ExpressionKind.Prefix:
        "Prefix\n" & @[self.op.treeRepr, self.expression.treeRepr].join("\n").indent(2)
    of ExpressionKind.Postfix:
        "Postfix\n" & @[self.op.treeRepr, self.expression.treeRepr].join("\n").indent(2)
    of ExpressionKind.Bracket:
        let args = self.args.join(", ")
        fmt"{self.callee}[{args}]"
    of ExpressionKind.Block:
        if self.label.isNone:
            &"block:\n{self.`block`}"
        else:
            &"block: {self.label.get}\n{self.`block`}"
    of ExpressionKind.Lambda:
        let
            params = $self.param
            suite = ($self.body).indent(2)
        fmt"func{params}:\n{suite}"
    of ExpressionKind.Malloc:
        fmt"malloc({self.mtype}, {self.msize})"
    of ExpressionKind.Typeof:
        fmt"Typeof\n  {self.`typeof`.treeRepr}"
    of ExpressionKind.Ref:
        fmt"Ref\n  {self.`ref`.treeRepr}"
    of ExpressionKind.FnType:
        let args = self.args.join(", ")
        fmt"func({args}) -> {self.rety}"
    of ExpressionKind.Fail:
        fmt"failed term"

proc treeRepr*(self: IdentDef): string =
    let
        typ = self.typ.map(treeRepr).get("None")
        default = self.default.map(treeRepr).get("None")
    "IdentDef\n" & (&"{self.pat}\n{typ}\n{default}").indent(2)

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
