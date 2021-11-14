
import sets
import options
import strformat
import strutils
import sequtils
import hashes
import tables

import coloredString

import lineinfos
import orders

import utils
import eat/utils as eutils

from llvm import Value


type
    TermKind* {.pure.} = enum
        Unit
        Bool
        Integer
        Float
        Char
        String
        Id
        Lambda
        List
        Tuple
        Record
        Let
        Var
        Const
        Typedef
        Funcdef
        If
        When
        Case
        While
        For
        Loop
        Block
        Asign
        Typeof
        Discard
        Apply
        Projection
        Meta
        Seq

    PatternKind* {.pure.} = enum
        Literal
        Ident
        Range
        Array
        Tuple
        Record
        Discard

    IdentDefs* = seq[IdentDef]
    IdentDef* = object
        id*: Ident
        typ*: Option[ref Term]
        default*: Option[ref Term]
    Ident* = ref Term  # suppose to be of TermKind.Id Pattern?
    # Ident* = object
    #     name*: string
    #     decl*: ref Term
        # typ*: ref Type
        # symbol*: Option[PSymbol]

    FunctionParam* = object
        gen*: IdentDefs
        params*: IdentDefs
        rety*: ref Term
    Function* = object
        id*: Ident  # string?
        param*: FunctionParam
        metadata*: Option[Metadata]
        body*: Body

    Body* = object
        term*: ref Term
        scope*: Scope

    Scope* = ref object
        parent*: Scope
        syms*: Table[string, seq[PSymbol]]
        typeOrder*: Order[ref Type]  # cumulative
        converters*: Table[(ref Type, ref Type), Ident]

    MetadataKind* {.pure.} = enum
        Link
        ImportLL
        Subtype
        Userdef
    Metadata* = object
        case kind*: MetadataKind
        of Link..Subtype:
            nil
        of Userdef:
            name*: string
        param*: ref Term
    BuildInMetadata* = Link..ImportLL
    FunctionMetadata* = ImportLL..ImportLL
    NoBodyMetadata* = ImportLL..ImportLL

    Pattern* = object
        case kind*: PatternKind
        of PatternKind.Literal:
            lit*: ref Term
        of PatternKind.Ident:
            id*: ref Term
        of PatternKind.Range:
            lb*: ref Term
            ub*: ref Term
        of PatternKind.Array:
            # TODO: pattern for array
            nil
        of PatternKind.Tuple:
            tpl*: seq[Pattern]
        of PatternKind.Record:
            # TODO: pattern for record
            nil
        of PatternKind.Discard:
            nil

    Term* = object
        loc*: Location
        typ*: ref Type
        case kind*: TermKind
        of Unit:
            nil
        of Bool:
            boolval*: bool
        of Integer:
            intval*: BiggestInt
        of Float:
            floatval*: BiggestFloat
        of Char:
            charval*: char
        of String:
            strval*: string
        of TermKind.Id:
            name*: string
        of Lambda:
            param*: IdentDefs
            body*: Body
        of TermKind.Tuple, TermKind.List:
            seqval*: seq[ref Term]
        of TermKind.Record:
            recordval*: seq[(Term, Term)]
        of Let, Var, Const:
            iddef*: IdentDef
        of Typedef:
            typedefs*: IdentDefs
        of Funcdef:
            fn*: Function
        of If, When:
            `elif`*: seq[(ref Term, Body)]
            `else`*: Body
        of Case:
            matcher*: ref Term
            branches*: seq[(Term, Term)]
        of While:
            cond*: ref Term
            wbody*: Body
        of Loop, Block:
            label*: Ident
            `block`*: Body
        of For, Asign:
            pat*: ref Term
            val*: ref Term
            forbody*: Body # for For
        of Typeof, TermKind.Discard:
            term*: ref Term
        of Apply:
            callee*: ref Term
            args*: seq[ref Term]
        of Projection:
            container*: ref Term
            index*: range[0..1]
        of TermKind.Meta:
            metadata*: Metadata
        of Seq:
            terms*: seq[ref Term]

    TypeKind* {.pure.} = enum
        Bottom
        Top
        Unit
        Bool
        Integer
        Float
        Char
        String
        List
        Pair
        # Tuple
        Record
        Arrow
        Typedesc
        Distinct
        Var
        Intersection
        Link
    PolyTypeKind* {.pure.} = enum
        Forall
        Intersection

    TypeVarId = int
    TypeVar* = object
        id*: TypeVarId
        lb*: ref Type
        ub*: ref Type

    DistinctTypeId = int
    Type* = object
        case kind*: TypeKind
        of Bottom, Top, TypeKind.Unit, TypeKind.Bool, TypeKind.Integer, TypeKind.Float, TypeKind.Char, TypeKind.String:
            nil
        of TypeKind.List:
            elem*: ref Type
        of TypeKind.Pair:
            first*: ref Type
            second*: ref Type
        # of TypeKind.Tuple, TypeKind.Intersection:
        of TypeKind.Intersection:
            types*: seq[ref Type]
        of TypeKind.Record:
            idtypes*: seq[(Ident, ref Type)]
        of Arrow:
            paramty*: seq[ref Type]
            rety*: ref Type
        of Typedesc:
            typ*: ref Type
        of Distinct:
            base*: ref Type
            id*: DistinctTypeId
        of TypeKind.Var:
            tv*: TypeVar
        of TypeKind.Link:
            to*: ref Type
        symbol*: Option[PSymbol]

    PolyType* = object
        case kind*: PolyTypeKind
        of Forall:
            gen*: HashSet[TypeVar]
            typ*: ref Type
        of PolyTypeKind.Intersection:
            types*: seq[PolyType]   # TODO: ref irukana? kanngaete

    SymbolKind* {.pure.} = enum
        Var
        Let
        Const
        Typ
        Func

    SymbolId = int
    PSymbol* = ref object
        id*: SymbolId
        kind*: SymbolKind
        global*: bool
        decl*: Ident   # assume be in IdentDef or Function
        ptyp*: PolyType
        impl*: ref Term
        use*: seq[ref Term]
        instances*: Table[ref Type, Symbol]
    Symbol* = ref object
        lty*: llvm.Type
        val*: Value


suite IdentDef:
    proc `$`*(self: ref Type): string
    proc `$`*(self: ref Term): string
    proc `$`*(self: IdentDef): string =
        result = $self.id.name
        if self.typ.isSome:
            result.add fmt": {self.typ.get}"
        elif self.id.typ.kind != TypeKind.Unit:
            result.add fmt"(: {self.id.typ})"
        if self.default.isSome:
            result.add fmt" = {self.default.get}"
    proc newIdentDef*(id: Ident): IdentDef =
        IdentDef(id: id, typ: none(ref Term), default: none(ref Term))
    proc newIdentDef*(id:Ident, typ: ref Term): IdentDef =
        IdentDef(id: id, typ: some(typ), default: none(ref Term))
    proc newIdentDef*(id:Ident, default: ref Term): IdentDef =
        IdentDef(id: id, typ: none(ref Term), default: some(default))
    proc newIdentDef*(id:Ident, typ: ref Term, default: ref Term): IdentDef =
        IdentDef(id: id, typ: some(typ), default: some(default))

suite Metadata:
    proc `$`*(self: Metadata): string =
        let name = case self.kind
        of MetadataKind.Link:
            "link"
        of MetadataKind.ImportLL:
            "importll"
        of MetadataKind.Subtype:
            "subtype"
        of MetadataKind.Userdef:
            self.name
        let param = if self.param.isNil: "" else: fmt": {self.param}"
        fmt"![{name}{param}]"
    proc Link*(_: typedesc[Metadata], param: ref Term): Metadata =
        Metadata(kind: MetadataKind.Link, param: param)
    proc ImportLL*(_: typedesc[Metadata], param: ref Term): Metadata =
        Metadata(kind: MetadataKind.ImportLL, param: param)
    proc Subtype*(_: typedesc[Metadata]): Metadata =
        Metadata(kind: MetadataKind.Subtype)
    proc Userdef*(_: typedesc[Metadata], name: string, param: ref Term): Metadata =
        Metadata(kind: MetadataKind.Userdef, param: param, name: name)

suite Body:
    proc `$`*(self: Body): string =
        $self.term
    proc newBody*(term: ref Term, scope: Scope): Body =
        Body(
            term: term,
            scope: scope
        )
suite FunctionParam:
    proc `$`*(self: FunctionParam): string =
        let
            gen = if self.gen.len == 0:
                ""
            else:
                let s = self.gen.map(`$`).join(", ")
                fmt"[{s}]"
            params = self.params.map(`$`).join(", ")
            rety = fmt" -> {self.rety}"
        fmt"{gen}({params}){rety}"

suite Function:
    proc `$`*(self: Function): string =
        let
            meta = if self.metadata.isSome: fmt" {self.metadata.get}" else: ""
            body = $self.body
        &"func {self.id.name}{self.param}{meta}:\n{body.indent(2)}"
    proc newFunction*(id: Ident, paramty: IdentDefs, rety: ref Term, body: Body, metadata: Option[Metadata] = none Metadata): Function =
        Function(id: id, param: FunctionParam(params: paramty, rety: rety), body: body, metadata: metadata)

suite TypeVar:
    proc `$`*(self: TypeVar): string =
        let m = len('a'..'z')
        var p = self.id
        while p >= m:
            let q = p mod m
            p = p div m
            result = chr(ord('a') + q) & result
        result = chr(ord('a') + p) & result
        result = fmt"'{result}(ub: {self.ub}, lb: {self.lb})"

suite Type:
    proc `==`*(self, other: ref Type): bool
    proc `==`*(self, other: Type): bool =
        ## equality as nim object
        if self.kind == TypeKind.Link:
            self.to[] == other
        elif other.kind == TypeKind.Link:
            self == other.to[]
        elif self.kind == other.kind:
            case self.kind
            of Bottom, Top, TypeKind.Unit, TypeKind.Bool, TypeKind.Integer, TypeKind.Float, TypeKind.Char, TypeKind.String:
                true
            of TypeKind.List:
                self.elem == other.elem
            of TypeKind.Pair:
                self.first == other.first and self.second == other.second
            # of TypeKind.Tuple, TypeKind.Intersection:
            of TypeKind.Intersection:
                self.types.zip(other.types).mapIt(it[0] == it[1]).foldl(a and b)
            of TypeKind.Record:
                self.idtypes.zip(other.idtypes).mapIt(it[0] == it[1]).foldl(a and b)
            of Arrow:
                self.paramty.zip(other.paramty).mapIt(it[0] == it[1]).foldl(a and b) and
                self.rety == other.rety
            of Typedesc:
                self.typ == other.typ
            of Distinct:
                self.base == other.base and self.id == other.id
            of TypeKind.Var:
                self.tv == other.tv
            of TypeKind.Link:
                self.to == other.to
        else:
            false
    proc `==`*(self, other: ref Type): bool =
        ## equality as nim object
        self[] == other[]
    proc `$`*(self: Type): string =
        case self.kind
        of TypeKind.Bottom:
            "Bottom"
        of TypeKind.Top:
            "Top"
        of TypeKind.Unit:
            "()"
        of TypeKind.Bool:
            # $"bool".red
            "bool"
        of TypeKind.Integer:
            # $"int".red
            "int"
        of TypeKind.Float:
            # $"float".red
            "float"
        of TypeKind.Char:
            # $"char".red
            "char"
        of TypeKind.String:
            # $"string".red
            "string"
        of TypeKind.List:
            fmt"[{self.base[]}]"
        of TypeKind.Pair:
            fmt"({self.first}, {self.second})"
        # of TypeKind.Tuple:
        #     "(" & self.types.mapIt($it[]).join(", ") & ")"
        of TypeKind.Record:
            "{" & self.idtypes.mapIt(fmt"{it[0]}: {it[1][]}").join(", ") & "}"
        of TypeKind.Arrow:
            self.paramty.mapIt($it[]).join(", ") & " -> " & $self.rety[]
        of TypeKind.Typedesc:
            # let t = "typedesc".red
            let t = "typedesc"
            fmt"{t}[{self.typ[]}]"
        of TypeKind.Var:
            $self.tv
        of TypeKind.Intersection:
            self.types.mapIt(
                if it.kind == TypeKind.Arrow:
                    fmt"({it[]})"
                else:
                    $it[]
            ).join("^")
        of TypeKind.Distinct:
            fmt"distinct {self.base[]}"
        of TypeKind.Link:
            $self.to
    proc `$`*(self: ref Type): string = $self[]
    var
        tvid: TypeVarId = -1
        dtid: DistinctTypeId = -1

    proc Bottom*(_: typedesc[Type]): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Bottom)
    proc Top*(_: typedesc[Type]): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Top)
    proc newTypeVar*(): TypeVar =
        inc tvid
        TypeVar(
            id: tvid,
            ub: Type.Top,
            lb: Type.Bottom
        )
    proc Unit*(_: typedesc[Type]): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Unit)
    proc Bool*(_: typedesc[Type]): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Bool)
    proc Integer*(_: typedesc[Type]): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Integer)
    proc Float*(_: typedesc[Type]): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Float)
    proc Char*(_: typedesc[Type]): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Char)
    proc String*(_: typedesc[Type]): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.String)
    proc List*(_: typedesc[Type], elem: ref Type): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.List, elem: elem)
    proc Pair*(_: typedesc[Type], first, second: ref Type): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Pair, first: first, second: second)
    # proc Tuple*(_: typedesc[Type], types: seq[ref Type]): ref Type =
    #     result = new Type
    #     result[] = Type(kind: TypeKind.Tuple, types: types)
    proc Record*(_: typedesc[Type], idtypes: seq[(Ident, ref Type)]): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Record, idtypes: idtypes)
    proc Arrow*(_: typedesc[Type], params: seq[ref Type], rety: ref Type): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Arrow, paramty: params, rety: rety)
    proc Typedesc*(_: typedesc[Type], typ: ref Type): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Typedesc, typ: typ)
    proc Distinct*(_: typedesc[Type], base: ref Type): ref Type =
        inc dtid
        result = new Type
        result[] = Type(kind: TypeKind.Intersection, base: base, id: dtid)
    proc Var*(_: typedesc[Type], tv: TypeVar): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Var, tv: tv)
    proc Var*(_: typedesc[Type]): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Var, tv: newTypeVar())
    proc Intersection*(_: typedesc[Type], types: seq[ref Type]): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Intersection, types: types)
    proc Link*(_: typedesc[Type], to: ref Type): ref Type =
        result = new Type
        result[] = Type(kind: TypeKind.Link, to: to)

    proc hasRegion*(self: ref Type): bool =
        case self.kind
        of TypeKind.Bottom..TypeKind.Char:
            false
        of TypeKind.String:
            true
        of TypeKind.Arrow:
            true
        of TypeKind.List:
            true
        of TypeKind.Pair:
            self.first.hasRegion or self.second.hasRegion
        # of TypeKind.Tuple, TypeKind.Intersection:
        of TypeKind.Intersection:
            self.types.any(hasRegion)
        of TypeKind.Record:
            self.idtypes.anyIt(it[1].hasRegion)
        of TypeKind.Distinct, TypeKind.Link:
            self.typ.hasRegion
        of TypeKind.Var, TypeKind.TypeDesc:
            assert false
            false

    proc compilable*(self: ref Type): bool =
        case self.kind
        of TypeKind.Bottom:
            false
        of TypeKind.Top..TypeKind.String:
            true
        of TypeKind.Arrow:
            self.paramty.all(compilable) and self.rety.compilable
        of TypeKind.List:
            self.elem.compilable
        of TypeKind.Pair:
            self.first.compilable and self.second.compilable
        # of TypeKind.Tuple:
        #     self.types.all(compilable)
        of TypeKind.Intersection:
            false
        of TypeKind.Record:
            self.idtypes.anyIt(it[1].compilable)
        of TypeKind.Distinct, TypeKind.Link:
            self.typ.compilable
        of TypeKind.Var, TypeKind.TypeDesc:
            assert false
            false

    proc hash*(self: TypeVar): Hash =
        result = !$ self.id
    proc hash*(self: ref Type): Hash =
        result = 0
        result = result !& self.kind.ord
        # TODO: implement hash of Type
        # case self.kind:
        # of TypeKind.Bottom:
        #     discard
        # of TypeKind.Top:
        #     discard
        # of TypeKind.Unit:
        #     discard
        # of TypeKind.Bool:
        #     discard
        # of TypeKind.Integer:
        #     discard
        # of TypeKind.Float:
        #     discard
        # of TypeKind.Char:
        #     discard
        # of TypeKind.String:
        #     discard
        # of TypeKind.List:
        #     discard
        # of TypeKind.Tuple:
        #     discard
        # of TypeKind.Record:
        #     discard
        # of TypeKind.Arrow:
        #     discard
        # of TypeKind.Typedesc:
        #     discard
        # of TypeKind.Var:
        #     discard
        # of TypeKind.Intersection:
        #     discard
        result = !$result

suite PolyType:
    proc Forall*(_: typedesc[PolyType], gen: HashSet[TypeVar], typ: ref Type): PolyType =
        PolyType(kind: PolyTypeKind.Forall, gen: gen, typ: typ)
    proc Intersection*(_: typedesc[PolyType], gen: HashSet[TypeVar], types: seq[PolyType]): PolyType =
        PolyType(kind: PolyTypeKind.Intersection, types: types)
    proc `$`*(self: PolyType): string =
        case self.kind
        of PolyTypeKind.ForAll:
            let tmp = if self.gen.len == 0: "" else: "∀" & toSeq(self.gen.items).map(`$`).join(".∀") & "."
            fmt"{tmp}{self.typ}"
        of PolyTypeKind.Intersection:
            self.types.join("∧")

suite Term:
    proc `$`*(self: ref Term): string =
        case self.kind
        of TermKind.Unit:
            "()"
        of TermKind.Bool:
            $self.boolval
        of TermKind.Integer:
            $self.intval
        of TermKind.Float:
            $self.floatval
        of TermKind.Char:
            $self.charval
        of TermKind.String:
            "\"" & self.strval & "\""
        of TermKind.Id:
            $self.name
        of TermKind.Lambda:
            fmt"λ{self.param}.{self.body}"
        of TermKind.List:
            let s = self.seqval.map(`$`).join(", ")
            fmt"[{s}]"
        of TermKind.Tuple:
            let s = self.seqval.map(`$`).join(", ")
            fmt"({s})"
        of TermKind.Record:
            # TODO: implement for record
            "not implemented"
        of TermKind.Let:
            fmt"let {self.iddef}"
        of TermKind.Var:
            fmt"var {self.iddef}"
        of TermKind.Const:
            fmt"const {self.iddef}"
        of TermKind.Typedef:
            let s = self.typedefs.map(`$`).join("\n")
            &"type\n{s.indent(2)}"
        of TermKind.Funcdef:
            $self.fn
        of TermKind.If:
            let
                elift = self.`elif`.mapIt(&"elif {it[0]}:\n{($it[1]).indent(2)}").join("\n")[2..^1]
                elset = ($self.`else`).indent(2)
            &"{elift}\nelse:\n{elset}"
        of TermKind.When:
            ""
        of TermKind.Case:
            ""
        of TermKind.While:
            ""
        of TermKind.For:
            ""
        of TermKind.Loop:
            let s = $self.body
            &"loop {self.label}\n{s.indent(2)}"
        of TermKind.Block:
            let s = $self.body
            &"block {self.label}\n{s.indent(2)}"
        of TermKind.Asign:
            fmt"{self.pat} = {self.val}"
        of TermKind.Typeof:
            fmt"typeof({self.term})"
        of TermKind.Discard:
            fmt"discard {self.term}"
        of TermKind.Apply:
            let
                args = self.args.join(", ")
            fmt"{self.callee}({args})"
        of TermKind.Projection:
            fmt"{self.container}.{self.index}"
        of TermKind.Meta:
            fmt"{self.metadata}"
        of TermKind.Seq:
            let f = proc(it: ref Term): string =
                let tmp = if it.typ.isNil or it.typ.kind == TypeKind.Unit:
                    ""
                else:
                    # if it.typ.region.isNil:
                    #     fmt" (: {it.typ})"
                    # else:
                    #     fmt" (: {it.typ}{it.typ.region})"
                    fmt" (: {it.typ})"
                fmt"{it}{tmp}"
            self.terms.map(f).join("\n")

    proc Unit*(_: typedesc[Term]): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Unit)
    proc Bool*(_: typedesc[Term], boolval: bool): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Bool, boolval: boolval)
    proc Integer*(_: typedesc[Term], intval: BiggestInt): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Integer, intval: intval)
    proc Float*(_: typedesc[Term], floatval: float): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Float, floatval: floatval)
    proc Char*(_: typedesc[Term], charval: char): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Char, charval: charval)
    proc String*(_: typedesc[Term], strval: string): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.String, strval: strval)
    proc Id*(_: typedesc[Term], name: string): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Id, name: name)
    proc Lambda*(_: typedesc[Term], param: IdentDefs, body: Body): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Lambda, param: param, body: body)
    proc List*(_: typedesc[Term]): ref Term =
        result = new Term
        result[] = Term()
    proc Tuple*(_: typedesc[Term], terms: seq[ref Term]): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Tuple, seqval: terms)
    proc Record*(_: typedesc[Term]): ref Term =
        result = new Term
        result[] = Term()
    proc Let*(_: typedesc[Term], iddef: IdentDef): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Let, iddef: iddef)
    proc Var*(_: typedesc[Term], iddef: IdentDef): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Var, iddef: iddef)
    proc Const*(_: typedesc[Term], iddef: IdentDef): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Const, iddef: iddef)
    proc Typedef*(_: typedesc[Term], iddefs: IdentDefs): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Typedef, typedefs: iddefs)
    proc Funcdef*(_: typedesc[Term], fn: Function): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Funcdef, fn: fn)
    proc If*(_: typedesc[Term], `elif`: seq[(ref Term, Body)], `else`: Body): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.If, `elif`: `elif`, `else`: `else`)
    proc When*(_: typedesc[Term]): ref Term =
        result = new Term
        result[] = Term()
    proc Case*(_: typedesc[Term]): ref Term =
        result = new Term
        result[] = Term()
    proc While*(_: typedesc[Term]): ref Term =
        result = new Term
        result[] = Term()
    proc For*(_: typedesc[Term]): ref Term =
        result = new Term
        result[] = Term()
    proc Loop*(_: typedesc[Term]): ref Term =
        result = new Term
        result[] = Term()
    proc Block*(_: typedesc[Term]): ref Term =
        result = new Term
        result[] = Term()
    proc Asign*(_: typedesc[Term], pat: ref Term, val: ref Term): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Asign, pat: pat, val: val)
    proc Typeof*(_: typedesc[Term], term: ref Term): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Typeof, term: term)
    proc Discard*(_: typedesc[Term], term: ref Term): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Discard, term: term)
    proc Apply*(_: typedesc[Term], callee: ref Term, args: seq[ref Term]): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Apply, callee: callee, args: args)
    proc Projection*(_: typedesc[Term], container: ref Term, index: range[0..1]): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Projection, container: container, index: index)
    proc Meta*(_: typedesc[Term], meta: Metadata): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Meta, metadata: meta)
    proc Seq*(_: typedesc[Term], terms: seq[ref Term]): ref Term =
        result = new Term
        result[] = Term(kind: TermKind.Seq, terms: terms)

    proc newIdent*(name: string): Ident =
        Term.Id(name)

suite Pattern:
    proc Literal*(_: typedesc[Pattern], lit: ref Term): Pattern =
        Pattern(kind: PatternKind.Literal, lit: lit)
    proc Id*(_: typedesc[Pattern], id: ref Term): Pattern =
        Pattern(kind: PatternKind.Ident, id: id)
suite PSymbol:
    proc `$`*(self: PSymbol): string =
        let
            kind = if self.kind == SymbolKind.Typ:
                "Type"
            else:
                $self.kind
            id = if self.global:
                $self.decl.red
            else:
                $self.decl.blue
            typ = $self.ptyp
            impl = if self.kind == SymbolKind.Func:
                "..."
            else:
                $self.impl
            loc = self.decl.loc
        fmt"{loc}: ({kind}){id}: {typ} (= {impl})"
    var
        symid = 0

    proc Let*(_: typedesc[PSymbol], ident: Ident, ptyp: PolyType, impl: ref Term, global: bool = false): PSymbol =
        inc symid
        result = PSymbol(
            kind: SymbolKind.Let, id: symid,
            decl: ident, ptyp: ptyp, impl: impl,
            global: global, use: @[]
        )
        result.ptyp.typ.symbol = some result
    proc Func*(_: typedesc[PSymbol], ident: Ident, ptyp: PolyType, impl: ref Term, global: bool = false): PSymbol =
        inc symid
        result = PSymbol(
            kind: SymbolKind.Func, id: symid,
            decl: ident, ptyp: ptyp, impl: impl,
            global: global, use: @[]
        )
        result.ptyp.typ.symbol = some result
    proc Typ*(_: typedesc[PSymbol], ident: Ident, ptyp: PolyType, impl: ref Term, global: bool = false): PSymbol =
        inc symid
        result = PSymbol(
            kind: SymbolKind.Typ, id: symid,
            decl: ident, ptyp: ptyp, impl: impl,
            global: global, use: @[]
        )
        result.ptyp.typ.symbol = some result

suite Symbol:
    proc `$`*(self: Symbol): string =
        $self[]

suite Scope:
    iterator items*(self: Scope): Scope =
        ## The first element is the youngest scope
        var scope = self
        while not scope.isNil:
            yield scope
            scope = scope.parent

    iterator reversed*(self: Scope): Scope =
        ## The first element is the oldest scope
        for scope in toSeq(self.items).reversed:
            yield scope

    proc `$`*(self: Scope): string =
        var
            tmp = self
            scopes: seq[Table[string, seq[PSymbol]]]
        while not tmp.isNil:
            scopes.add tmp.syms
            tmp = tmp.parent
        if scopes.foldl(a + b.len, 0) == 0:
            return "{}"
        for scope in scopes:
            for (key, val) in scope.pairs:
                result &= &"\"{key}\": {val},\n"
        result = &"{{\n{result[0..^3].indent(2)}\n}}"

    proc newScope*(parent: Scope = nil): Scope =
        Scope(
            parent: parent,
            syms: initTable[string, seq[PSymbol]](),
            typeOrder: if parent.isNil: newOrder[ref Type]() else: parent.typeOrder,
            converters: initTable[(ref Type, ref Type), Ident]()
        )
