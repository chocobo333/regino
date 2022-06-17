
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


proc `$`*(self: Statement, typed: bool = false, regioned: bool = false, comment: bool = false): string
proc `$`*(self: Expression, typed: bool = false, regioned: bool = false, comment: bool = false): string
proc `$`*(self: FunctionParam, typed: bool = false, regioned: bool = false, comment: bool = false): string
proc `$`*(self: TypeExpression, typed: bool = false, regioned: bool = false, comment: bool = false): string
proc `$`*(self: Pattern, typed: bool = false, regioned: bool = false, comment: bool = false): string
proc `$`*(self: Suite, typed: bool = false, regioned: bool = false, comment: bool = false): string
proc `$`*(self: Value, typed: bool = false, regioned: bool = false, comment: bool = false): string
proc `$`*(self: Region, typed: bool = false, regioned: bool = false, comment: bool = false): string
proc `$`*(self: Comment, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    if self.isDoc: "##" & self.s else: "#" & self.s
proc `$`*(self: seq[Comment], typed: bool = false, regioned: bool = false, comment: bool = false): string =
    if comment: self.mapIt(`$`(it, typed, regioned, comment)).join("\n")
    else: self.filterIt(it.isDoc).mapIt(`$`(it, typed, regioned, comment)).join("\n")
proc `$`*(self: Literal, typed: bool = false, regioned: bool = false, comment: bool = false): string =
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
proc `$`*(self: Ident, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        re = re"(*UTF8)^[_\p{L}\p{Nl}ー][_\p{L}\p{N}ー]*$"
    if self.name.match(re).isSome:
        self.name
    else:
        fmt"`{self.name}`"
proc `$$`*(self: Ident, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    self.name
proc `$`*(self: Suite, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    self.stmts.mapIt(`$`(it, typed, regioned, comment)).join("\n").indent(2)
proc `$`*(self: ElifBranch, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        cond = `$`(self.cond, typed, regioned, comment)
        suite = `$`(self.suite, typed, regioned, comment)
    &"elif {cond}:\n{suite}"
proc `$`*(self: OfBranch, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        pat = `$`(self.pat, typed, regioned, comment)
        suite = `$`(self.suite, typed, regioned, comment)
    &"of {pat}:\n{suite}"
proc `$`*(self: Expression, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    result = case self.kind
    of ExpressionKind.Literal:
        `$`(self.litval, typed, regioned, comment)
    of ExpressionKind.Ident:
        `$`(self.ident, typed, regioned, comment)
    of ExpressionKind.Tuple:
        let s = self.exprs.mapIt(`$`(it, typed, regioned, comment)).join(", ")
        fmt"({s})"
    of ExpressionKind.Array:
        let s = self.exprs.mapIt(`$`(it, typed, regioned, comment)).join(", ")
        fmt"[{s}]"
    of ExpressionKind.Record:
        let members = self.members.mapIt(
            fmt"{`$`(it[0], typed, regioned, comment)}: " &
            fmt"{`$`(it[1], typed, regioned, comment)}"
        ).join(", ")
        fmt"({members})"
    of ExpressionKind.ObjCons:
        let
            typname = `$`(self.typname, typed, regioned, comment)
            members = self.members.mapIt(
                fmt"{`$`(it[0], typed, regioned, comment)}: " &
                fmt"{`$`(it[1], typed, regioned, comment)}"
            ).join(", ")
        fmt"{typname}({members})"
    of ExpressionKind.If:
        let
            elifs = self.elifs.mapIt(`$`(it, typed, regioned, comment)).join("\n")[2..^1]
            elseb = if self.elseb.isSome:
                let elseb = `$`(self.elseb.get, typed, regioned, comment)
                &"\nelse\n{elseb}"
            else:
                ""
        &"{elifs}{elseb}"
    of ExpressionKind.When:
        let
            elifs = "when" & self.elifs.map(`$`).join("\n")[4..^1]
            elseb = if self.elseb.isSome:
                let elseb = `$`(self.elseb.get, typed, regioned, comment)
                &"\nelse\n{elseb}"
            else:
                ""
        &"{elifs}{elseb}"
    of ExpressionKind.Case:
        let
            val = `$`(self.val, typed, regioned, comment)
            ofs = if self.ofs.len == 0: "" else: ("\n" & self.ofs.map(`$`).join("\n"))
            default = "\n" & self.default.map(
                    it => "default:\n" & `$`(it, typed, regioned, comment)
                ).get("")
        fmt"case {val}{ofs}{default}"
    of ExpressionKind.Call:
        let
            callee = `$`(self.callee, typed, regioned, comment)
            args = self.args.mapIt(`$`(it, typed, regioned, comment)).join(", ")
        fmt"{callee}({args})"
    of ExpressionKind.Command:
        let
            callee = `$`(self.callee, typed, regioned, comment)
            args = self.args.mapIt(`$`(it, typed, regioned, comment)).join(", ")
        fmt"{callee} {args}"
    of ExpressionKind.Dot:
        let
            lhs = `$`(self.lhs, typed, regioned, comment)
            rhs = `$`(self.rhs, typed, regioned, comment)
        fmt"{lhs}.{rhs}"
    of ExpressionKind.Binary:
        let
            op = `$$`(self.op, typed, regioned, comment)
            lhs = `$`(self.lhs, typed, regioned, comment)
            rhs = `$`(self.rhs, typed, regioned, comment)
        fmt"{lhs} {op} {rhs}"
    of ExpressionKind.Prefix:
        let
            op = `$$`(self.op, typed, regioned, comment)
            typ = if typed: fmt"(:{`$`(self.typ, typed, regioned, comment)})" else: ""
            expression = `$`(self.expression, typed, regioned, comment)
        fmt"{op}{typ}{expression}"
    of ExpressionKind.Postfix:
        let
            expression = `$`(self.expression, typed, regioned, comment)
            op = `$$`(self.op, typed, regioned, comment)
            typ = if typed: fmt"(:{`$`(self.op.typ, typed, regioned, comment)})" else: ""
        fmt"{expression}{op}{typ}"
    of ExpressionKind.Block:
        if self.label.isNone:
            let blck = `$`(self.`block`, typed, regioned, comment)
            &"block:\n{blck}"
        else:
            let
                label = `$`(self.label.get, typed, regioned, comment)
                blck = `$`(self.`block`, typed, regioned, comment)
            &"block: {label}\n{blck}"
    of ExpressionKind.Bracket:
        let
            callee = `$`(self.callee, typed, regioned, comment)
            args = self.args.mapIt(`$`(it, typed, regioned, comment)).join(", ")
        fmt"{callee}[{args}]"
    of ExpressionKind.Lambda:
        let
            params = `$`(self.param, typed, regioned, comment)
            suite = `$`(self.body, typed, regioned, comment)
        if suite.count('\n') == 0:
            &"func{params}: {suite[2..^1]}"
        else:
            &"func{params}:\n{suite}"
    of ExpressionKind.Malloc:
        let
            mtype = `$`(self.mtype, typed, regioned, comment)
            msize = `$`(self.msize, typed, regioned, comment)
        fmt"malloc({mtype}, {msize})"
    of ExpressionKind.Typeof:
        let typeof = `$`(self.typeof, typed, regioned, comment)
        fmt"typeof({typeof})"
    of ExpressionKind.Ref:
        let rf = `$`(self.`ref`, typed, regioned, comment)
        fmt"ref {rf}"
    of ExpressionKind.FnType:
        let
            rety = `$`(self.rety, typed, regioned, comment)
            args = self.args.mapIt(`$`(it, typed, regioned, comment)).join(", ")
        fmt"func({args}) -> {rety}"
    of ExpressionKind.IntCast:
        let
            int_exp = `$`(self.int_exp, typed, regioned, comment)
        fmt"cast({self.int_exp}, {self.from}, {self.to})"
    of ExpressionKind.Fail:
        fmt"failed term"
    if typed and not self.typ.isNil:
        let typ = `$`(self.typ, typed, regioned, comment)
        result = fmt"{result} (: {typ})"

proc `$`*(self: Metadata, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let params =
        if self.params.len == 0:
            ""
        else:
            let params = self.params.mapIt(`$`(it, typed, regioned, comment)).join("\n")
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
proc `$`*(self: Pattern, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    case self.kind
    of PatternKind.Literal:
        `$`(self.litval, typed, regioned, comment)
    of PatternKind.Ident:
        `$`(self.ident, typed, regioned, comment)
    # of PatternKind.Bracket:
    #     let args = self.args.join(", ")
    #     fmt"{self.callee}[{args}]"
    of PatternKind.Tuple:
        let
            tag = self.tag.map(it => `$`(it, typed, regioned, comment)).get("")
            s = self.patterns.mapIt(`$`(it, typed, regioned, comment)).join(", ")
        fmt"{tag}({s})"
    of PatternKind.Record:
        let
            tag = self.tag.map(it => `$`(it, typed, regioned, comment)).get("")
            members = self.members.mapIt(
                    fmt"{`$`(it[0], typed, regioned, comment)}:" &
                    fmt" {`$`(it[0], typed, regioned, comment)}"
                ).join(", ")
        fmt"{tag}({members})"
    of PatternKind.UnderScore:
        "_"
proc `$`*(self: IdentDef, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        pat = `$`(self.pat, typed, regioned, comment)
        typ = if self.typ.isNone: "" else: fmt": {`$`(self.typ.get, typed, regioned, comment)}"
        default = if self.default.isNone: "" else: fmt" = {`$`(self.default.get, typed, regioned, comment)}"
        comments = if self.comments.len != 0: "\n" & fmt"{`$`(self.comments, typed, regioned, comment)}" else: ""
    fmt"{pat}{typ}{default}{comments}"
proc `$`*(self: GenTypeDef, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        id = `$`(self.id, typed, regioned, comment)
        ub = if self.ub.isSome: fmt" <: {`$`(self.ub.get, typed, regioned, comment)}" else: ""
    fmt"{id}{ub}"
proc `$`*(self: TypeDef, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        id = `$`(self.id, typed, regioned, comment)
        params =
            if self.params.isNone:
                ""
            else:
                let params = self.params.get.map(it => `$`(it, typed, regioned, comment)).join(", ")
                fmt"[{params}]"
        typ = `$`(self.typ, typed, regioned, comment)
        comments = if self.comments.len != 0: "\n" & fmt"{`$`(self.comments, typed, regioned, comment)}" else: ""
    fmt"{id}{params} = {typ}{comments}"
proc `$`*(self: FunctionParam, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        implicit = block:
            let imp = self.implicit.mapIt(`$`(it, typed, regioned, comment)).join(", ")
            if self.implicit.len == 0:
                ""
            else:
                fmt"[{imp}]"
        params = self.params.mapIt(`$`(it, typed, regioned, comment)).join(", ")
        rety = if self.rety.isNone: ""
               else: fmt" -> {`$`(self.rety.get, typed, regioned, comment)}"
    fmt"{implicit}({params}){rety}"

proc `$`*(self: Function, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        fn = ["func", "prop"][int self.isProp]
        id = `$`(self.id, typed, regioned, comment)
        param = `$`(self.param, typed, regioned, comment)
        metadata = if self.metadata.isNone: "" else: fmt" {self.metadata.get}"
        suite = if self.suite.isNone: "" else: ":\n" & fmt"{`$`(self.suite.get, typed, regioned, comment)}"
    &"{fn} {id}{param}{metadata}{suite}"

proc `$`*(self: IdentDefSection, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        # comments = if comment and self.comments.len != 0: self.comments.mapIt(`$`(it, typed, regioned, comment)).join("\n") & "\n" else: ""
        comments = if self.comments.len != 0: self.comments.mapIt(`$`(it, typed, regioned, comment)).join("\n") & "\n" else: ""
        iddefs = self.iddefs.mapIt(`$`(it, typed, regioned, comment)).join("\n")
    fmt"{comments}{iddefs}"
proc `$`*(self: TypeDefSection, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        # comments = if comment and self.comments.len != 0: self.comments.mapIt(`$`(it, typed, regioned, comment)).join("\n") & "\n" else: ""
        comments = if self.comments.len != 0: self.comments.mapIt(`$`(it, typed, regioned, comment)).join("\n") & "\n" else: ""
        typedefs = self.typedefs.mapIt(`$`(it, typed, regioned, comment)).join("\n")
    fmt"{comments}{typedefs}"
proc `$`*(self: Statement, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    case self.kind
    of StatementKind.For:
        let
            pat = `$`(self.pat, typed, regioned, comment)
            val = `$`(self.val, typed, regioned, comment)
            suite = `$`(self.suite, typed, regioned, comment)
        &"for {pat} in {val}:\n{suite}"
    of StatementKind.While:
        "while" & ($self.branch)[4..^1]
    of StatementKind.Loop:
        if self.label.isNone:
            let blck = `$`(self.`block`, typed, regioned, comment)
            &"loop\n{blck}"
        else:
            let
                label = `$`(self.label.get, typed, regioned, comment)
                blck = `$`(self.`block`, typed, regioned, comment)
            &"loop {label}\n{blck}"
    of StatementKind.LetSection:
        let iddefSection = `$`(self.iddefSection, typed, regioned, comment)
        "let\n" & fmt"{iddefSection}".indent(2)
    of StatementKind.VarSection:
        let iddefSection = `$`(self.iddefSection, typed, regioned, comment)
        "var\n" & fmt"{iddefSection}".indent(2)
    of StatementKind.ConstSection:
        let iddefSection = `$`(self.iddefSection, typed, regioned, comment)
        "const\n" & fmt"{iddefSection}".indent(2)
        # let iddefs = self.iddefs.map(`$`).join("\n").indent(2)
        # &"const\n{iddefs}"
    of StatementKind.TypeSection:
        let typedefSection = `$`(self.typedefSection, typed, regioned, comment)
        "type\n" & fmt"{typedefSection}".indent(2)
        # let typedefs = self.typedefs.map(`$`).join("\n").indent(2)
        # &"type\n{typedefs}"
    of StatementKind.Asign:
        let
            pat = `$`(self.pat, typed, regioned, comment)
            val = `$`(self.val, typed, regioned, comment)
        fmt"{pat} = {val}"
    of StatementKind.IndexAssign:
        fmt"{self.id}[{self.index}] = {self.i_val}"
    of StatementKind.Funcdef:
        `$`(self.fn, typed, regioned, comment)
    of StatementKind.Meta:
        `$`(self.meta, typed, regioned, comment)
    of StatementKind.Discard:
        if self.`discard`.isSome:
            let dscrd = `$`(self.`discard`.get, typed, regioned, comment)
            fmt"discard {dscrd}"
        else:
            fmt"discard"
    of StatementKind.Comments:
        `$`(self.comments, typed, regioned, comment)
    of StatementKind.Expression:
        `$`(self.expression, typed, regioned, comment)
    of StatementKind.Fail:
        "failed term"
proc `$`*(self: SumConstructor, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    `$`(self.id, typed, regioned, comment) & (
        case self.kind
        of SumConstructorKind.NoField:
            ""
        of SumConstructorKind.UnnamedField:
            let types = self.types.mapIt(`$`(it, typed, regioned, comment)).join(", ")
            fmt"({types})"
        of SumConstructorKind.NamedField:
            proc `$`(field: (Ident, Expression), typed: bool = false, regioned: bool = false, comment: bool = false): string =
                let
                    id = `$`(field[0], typed, regioned, comment)
                    typ = `$`(field[1], typed, regioned, comment)
                fmt"{id}: {typ}"
            let fields = self.fields.map(`$`).join(", ")
            fmt"({fields})"
    )
proc `$`*(self: SumType, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let cons = self.constructors.mapIt(`$`(it, typed, regioned, comment)).join("\n").indent(2)
    &"variant\n{cons}"
proc `$`*(self: Trait, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    case self.kind
    of TraitKind.Is:
        let
            val = `$`(self.val, typed, regioned, comment)
            pat = `$`(self.pat, typed, regioned, comment)
        fmt"{pat} is {val}"
    of TraitKind.Func:
        let fn = `$`(self.fn, typed, regioned, comment)
        fmt"{fn}"

proc `$`*(self: TraitType, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        pat = `$`(self.pat, typed, regioned, comment)
        typ = `$`(self.typ, typed, regioned, comment)
        traits =
            if self.traits.len == 0:
                ""
            else:
                "\n" & self.traits.mapIt(`$`(it, typed, regioned, comment)).join("\n").indent(2)
    &"trait {pat}: {typ}{traits}"
proc `$`*(self: TypeExpression, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    result = if self.isRef:
        "ref "
    else:
        ""
    result &= (
        case self.kind
        of TypeExpressionKind.Object:
            let
                members = self.members.mapIt(
                    fmt"{`$`(it[0], typed, regioned, comment)}: " &
                    fmt"{`$`(it[1], typed, regioned, comment)}"
                ).join("\n").indent(2)
            &"object\n{members}"
        of TypeExpressionKind.Sum:
            $self.sum
        of TypeExpressionKind.Distinct:
            let base = `$`(self.base, typed, regioned, comment)
            fmt"distinct {base}"
        of TypeExpressionKind.Trait:
            `$`(self.trait, typed, regioned, comment)
        of TypeExpressionKind.Expression:
            `$`(self.expression, typed, regioned, comment)
    )
proc `$`*(self: Program, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    self.stmts.mapIt(`$`(it, typed, regioned, comment)).join("\n")

proc id2s(self: int, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        c = toSeq('a'..'z')
        n = c.len
    var
        id = self - 1
    while id > n:
        let
            index = id mod n
        result.add c[index]
        id = id div n
    result.add c[id mod n]
    result.reverse
proc `$`*(self: TypeVar, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        id = id2s(self.id, typed, regioned, comment)
        lb = `$`(self.lb, typed, regioned, comment)
        ub = `$`(self.ub, typed, regioned, comment)
    result = fmt"'{id}({lb}, {ub})"
proc `$`*(self: GenericType, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    self.ident.name
proc `$$`*(self: GenericType, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let ub = if self.ub.kind != ValueKind.Unit: fmt" <: {`$`(self.ub, typed, regioned, comment)}" else: ""
    fmt"{self.ident.name}{ub}"
proc `$`*(self: Value, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    result = case self.kind
    of ValueKind.Literal:
        `$`(self.litval, typed, regioned, comment)
    of ValueKind.Bottom:
        "Bottom"
    of ValueKind.Unit:
        "()"
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
        let
            first = `$`(self.first, typed, regioned, comment)
            second = `$`(self.second, typed, regioned, comment)
        fmt"({first}, {second})"
    of ValueKind.Array:
        let base = `$`(self.base, typed, regioned, comment)
        fmt"array[{base}]"
    of ValueKind.ArrayV:
        let vals = self.vals.mapIt(`$`(it, typed, regioned, comment)).join(", ")
        fmt"[{vals}]"
    of ValueKind.Record:
        var res = "("
        for (id, val) in self.members.pairs:
            let
                id = `$`(id, typed, regioned, comment)
                val = `$`(val, typed, regioned, comment)
            res.add fmt"{id}: {val}, "
        res[0..^3] & ")"
    of ValueKind.Ptr:
        let pointee = `$`(self.pointee, typed, regioned, comment)
        fmt"ptr {pointee}"
    of ValueKind.Pi:
        let
            imp =
                if self.implicit.len == 0:
                    ""
                else:
                    let imp = self.implicit.mapIt(`$$`(it, typed, regioned, comment)).join(", ")
                    fmt"[{imp}]"
            params =
                if self.params.len == 0:
                    ""
                else:
                    let params = self.params.mapIt(`$`(it, typed, regioned, comment)).join(", ")
                    fmt"({params})"
            rety = `$`(self.rety, typed, regioned, comment)
        fmt"{imp}{params} -> {rety}"
    of ValueKind.Family:
        let
            imp = self.implicit.mapIt(`$$`(it, typed, regioned, comment)).join(", ")
            rety = `$`(self.rety, typed, regioned, comment)
        fmt"[{imp}]{rety}"
    of ValueKind.Sum:
        var res = "variant\n"
        for (id, val) in self.constructors.pairs:
            let
                id = `$`(id, typed, regioned, comment)
                val = `$`(val, typed, regioned, comment)
            res.add &"  {id}{val}\n"
        res = res[0..^2]
        res
    of ValueKind.Trait:
        "Trait"
    of ValueKind.Singleton:
        let base = `$`(self.base, typed, regioned, comment)
        fmt"sigleton[{base}]"
    of ValueKind.Distinct:
        self.ident.name
        # case self.base.kind
        # of ValueKind.Record:
        #     var members: string
        #     for (id, val) in self.base.members.pairs:
        #         members.add &"\n  {id}: {val}"
        #     &"{self.ident}{members}"
        # else:
        #     fmt"distinct {self.base}"
    of ValueKind.Intersection:
        toSeq(self.types).mapIt(`$`(it, typed, regioned, comment)).join("^")
    of ValueKind.Union:
        toSeq(self.types).mapIt(`$`(it, typed, regioned, comment)).join"\/"
    of ValueKind.Select:
        let
            id = self.id.id2s(typed, regioned, comment)
            s = toSeq(self.types).mapIt(`$`(it, typed, regioned, comment)).join(" or ")
        fmt"'{id}({s})"
    of ValueKind.Lambda:
        let
            params = self.l_param.mapIt(`$`(it, typed, regioned, comment)).join(", ")
            suite = `$`(self.suite, typed, regioned, comment)
        &"lambda {params}: \n{suite}"
    of ValueKind.Cons:
        let
            constructor = `$`(self.constructor, typed, regioned, comment)
            args = self.args.mapIt(`$`(it, typed, regioned, comment)).join", "
        fmt"{constructor}[{args}]"
    of ValueKind.Var:
        `$`(self.tv, typed, regioned, comment)
    of ValueKind.Gen:
        `$`(self.gt, typed, regioned, comment)
    of ValueKind.Link:
        let to = `$`(self.to, typed, regioned, comment)
        fmt"~{to}"
    if  regioned and not self.region.isNil:
        let region = `$`(self.region, typed, regioned, comment)
        result = fmt"({result}, {region})"

proc `$`*(self: Symbol, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    let
        kind =
            if self.kind == SymbolKind.Typ:
                "Type"
            else:
                $self.kind
        id = if self.global:
            `$`(self.id, typed, regioned, comment)
        else:
            `$`(self.id, typed, regioned, comment)
        typ = `$`(self.typ, typed, regioned, comment)
        impl =
            case self.kind
            of SymbolKind.Func:
                "..."
            of SymbolKind.Var..SymbolKind.Param:
                `$`(self.decl_iddef, typed, regioned, comment)
            of SymbolKind.Typ:
                `$`(self.decl_typedef, typed, regioned, comment)
            of SymbolKind.GenParam:
                `$`(self.decl_gendef, typed, regioned, comment)
            of SymbolKind.Field:
                "(" &
                `$`(self.fielddef[0], typed, regioned, comment) &
                ", " &
                `$`(self.fielddef[1], typed, regioned, comment) &
                ")"
            of SymbolKind.Enum:
                `$`(self.enumdef, typed, regioned, comment)
        loc = self.id.loc
    fmt"{loc}: ({kind}){id}: {typ} ({impl})"
proc `$`*(self: Scope, typed: bool = false, regioned: bool = false, comment: bool = false): string =
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
                            `$`(val[0], typed, regioned, comment)
                        else:
                            val.mapIt(`$`(it, typed, regioned, comment)).join("\n").indent(2)
                result &= &"\"{key}\": {val},\n"
        &"{{\n{result[0..^3].indent(2)}\n}}"
    result.add "\n"
    result.add if consts.foldl(a + b.len, 0) == 0:
        "{}"
    else:
        var ret: string
        for scope in consts:
            for (key, val) in scope.pairs:
                discard `$`(@[1, 2])
                let
                    val = val.mapIt(`$`(it, typed, regioned, comment)).join(", ")
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
        let s = self.exprs.mapIt(`$`(it)).join(", ")
        fmt"({s})"
    of ExpressionKind.Array:
        "Array\n" & self.exprs.map(treeRepr).join("\n").indent(2)
    of ExpressionKind.Record:
        let members = self.members.mapIt(fmt"{it[0]}: {it[1]}").join(", ")
        fmt"({members})"
    of ExpressionKind.ObjCons:
        let
            typname = fmt"{self.typname}"
            members = self.members.mapIt(fmt"{it[0]}: {it[1]}").join(", ")
        fmt"{typname}({members})"
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
        let args = self.args.mapIt(`$`(it)).join(", ")
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
        let args = self.args.mapIt(`$`(it)).join(", ")
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
        let args = self.args.mapIt(`$`(it)).join(", ")
        fmt"func({args}) -> {self.rety}"
    of ExpressionKind.IntCast:
        $self
    of ExpressionKind.Fail:
        fmt"failed term"

proc treeRepr*(self: IdentDef): string =
    let
        typ = self.typ.map(treeRepr).get("None")
        default = self.default.map(treeRepr).get("None")
    "IdentDef\n" & (&"{self.pat}\n{typ}\n{default}").indent(2)

proc sub2(self: int): string =
    [
        "\u2080",
        "\u2081",
        "\u2082",
        "\u2083",
        "\u2084",
        "\u2085",
        "\u2086",
        "\u2087",
        "\u2088",
        "\u2089",
    ][self]
proc sub(self: int): string =
    var
        p = self
    while p >= 10:
        result = sub2(p mod 10) & result
        p = p div 10
    result = sub2(p) & result
proc `$`*(self: Region, typed: bool = false, regioned: bool = false, comment: bool = false): string =
    case self.kind
    of RegionKind.Static:
        "static"
    of RegionKind.Global:
        "global"
    of RegionKind.Param:
        fmt"p{sub(self.nth)}"
    of RegionKind.Return:
        "return"
    of RegionKind.Suite:
        let parent = `$`(self.parent, typed, regioned, comment)
        fmt"suite({parent})"
    of RegionKind.Var:
        fmt"ρ{sub(self.id)}"
    of RegionKind.Link:
        `$`(self.to, typed, regioned, comment)

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
