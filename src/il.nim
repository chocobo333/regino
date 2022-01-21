
import sets
import strformat
import strutils
import sequtils
import hashes
import tables
import options

import coloredString

import lineinfos
import orders

import utils


from llvm import Value

# {.experimental: "notnil".}


type
    TermKind* {.pure.} = enum
        Failed
        bottom
        `()`    # is term, `()`: Unit: U
        Unit    # is type
        U       # Universe_0 or Type_0
        Bool
        Integer
        Float
        Char
        String
        Id
        # Lambda
        # List
        Tuple
        Record  # named tuple
        Let
        Var
        Const
        # Typedef
        Funcdef
        FuncdefInst
        FunctionInst
        If
        # When
        Case
        # While
        # For
        Loop
        Block
        Asign
        Typeof
        Malloc
        Discard
        Apply
        # Projection
        Meta
        Seq
        Us

    IdentDefs* = seq[IdentDef]
    IdentDef* = object
        pat*: Term
        typ*: Option[Term]
        default*: Option[Term]
    Ident* = ref TermObject # not nil  # suppose to be of TermKind.Id Pattern?
    # Ident* = object
    #     name*: string
    #     decl*: Term
        # typ*: ref Type
        # symbol*: Option[PSymbol]

    FunctionParam* = object
        gen*: IdentDefs
        params*: IdentDefs
        rety*: Term
    Function* = object
        id*: Ident  # string?
        param*: FunctionParam
        metadata*: Option[Metadata]
        body*: Body

    Body* = object
        term*: Term
        scope*: Scope

    Scope* = ref object
        parent*: Scope
        syms*: Table[string, seq[Symbol]]
        consts*: Table[string, seq[Symbol]]
        typeOrder*: Order[ref Value]  # cumulative
        converters*: Table[(ref Value, ref Value), Ident]

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
        param*: seq[Term]
    BuildInMetadata* = Link..ImportLL
    FunctionMetadata* = ImportLL..ImportLL
    NoBodyMetadata* = ImportLL..ImportLL

    Term* = ref TermObject # not nil
    TermObject = object
        loc*: Location
        typ*: ref Value
        inserted*: bool
        case kind*: TermKind
        of TermKind.Failed:
            nil
        of TermKind.bottom:
            nil
        of TermKind.`()`, TermKind.Unit:
            nil
        of TermKind.U:
            level*: int
        of Bool:
            boolval*: bool
        of Integer:
            intval*: BiggestInt
            bits*: uint
        of Float:
            floatval*: BiggestFloat
        of Char:
            charval*: char
        of String:
            strval*: string
        of TermKind.Id:
            name*: string
        # of Lambda:
        #     param*: IdentDefs
        #     body*: Body
        # of Seq, TermKind.Tuple, TermKind.List:
        of Seq, TermKind.Tuple:
            terms*: seq[Term]
        of TermKind.Record:
            members*: Table[string, Term]
        of Let, Var, Const:
            iddef*: IdentDef
        # of Typedef:
        #     typedefs*: IdentDefs
        of Funcdef, FuncdefInst:
            fn*: Function
        of FunctionInst:
            pfn*: Term
            instargs*: seq[Term]
        of If: # When
            `elif`*: seq[(Term, Body)]
            `else`*: Body
        of Case:
            matcher*: Term
            branches*: seq[(Term, Term)]
        # of While:
        #     cond*: Term
        #     wbody*: Body
        of Loop, Block:
            label*: Ident
            `block`*: Body
        # of For, Asign:
        of Asign:
            pat*: Term
            val*: Term
            # forbody*: Body # for For
        of Typeof, TermKind.Discard:
            term*: Term
        of Malloc:
            malloctype*: Term
            mallocsize*: Term
        of Apply:
            callee*: Term
            args*: seq[Term]
        # of Projection:
        #     container*: Term
        #     index*: range[0..1]
        of TermKind.Meta:
            metadata*: Metadata
        of TermKind.Us:
            nil

    ValueKind* {.pure.} = enum
        Bottom
        `()` # (): unit: U
        Unit
        U
        Bool
        Integer
        Float
        Char
        String
        # List
        Ptr
        Pair
        # Tuple
        Record  # named tuple
        Pi
        # Sigma
        Typedesc # singleton
        # Distinct
        Var
        Intersection
        Union
        Link
        Gen
        BoolV
        IntV
        FloatV
        CharV
        StringV

    TypeVarId = int
    TypeVar* = object
        id*: TypeVarId
        lb*: ref Value
        ub*: ref Value

    DistinctTypeId = int
    Value* = object
        # name: string # TODO:
        case kind*: ValueKind
        # of Bottom, Top, ValueKind.Unit, ValueKind.Bool, ValueKind.Integer, ValueKind.Float, ValueKind.Char, ValueKind.String:
        of Bottom, ValueKind.`()`, ValueKind.Unit, ValueKind.Bool, ValueKind.Float, ValueKind.Char, ValueKind.String:
            nil
        of ValueKind.Integer:
            bits*: uint
        of ValueKind.U:
            level*: int
        # of ValueKind.List:
        #     elem*: ref Value
        of ValueKind.Ptr:
            pointee*: ref Value
        of ValueKind.Pair:
            first*: ref Value
            second*: ref Value
        of ValueKind.Record:
            members*: Table[string, ref Value]
        # of ValueKind.Tuple, ValueKind.Intersection:
        of ValueKind.Intersection, ValueKind.Union:
            types*: HashSet[ref Value]
        # of ValueKind.Record:
        #     idtypes*: seq[(Ident, ref Value)]
        # of Arrow:
        #     paramty*: seq[ref Value]
        #     rety*: ref Value
        of ValueKind.Pi:
            genty*: seq[(Term, ref Value)]
            gentyinst*: seq[ref Value] # for type inference
            paramty*: seq[ref Value]
            rety*: ref Value
        # of ValueKind.Sigma:
        #     first*: ref Value
        #     second*: ref Value
        of Typedesc:
            `typedesc`*: ref Value
        # of Distinct:
        #     base*: ref Value
        #     id*: DistinctTypeId
        of ValueKind.Var:
            tv*: TypeVar
        of ValueKind.Link:
            to*: ref Value
        of ValueKind.Gen:
            gen*: GenType
        of ValueKind.BoolV:
            boolval: bool
        of ValueKind.IntV:
            intval*: BiggestInt
        of ValueKind.FloatV:
            floatval*: BiggestFloat
        of ValueKind.CharV:
            charval*: char
        of ValueKind.StringV:
            strval*: string
        symbol*: Option[Symbol]

    GenType* = object
        name*: string
        ub*: ref Value
        typ*: ref Value

    # PolyType* = object
    #     case kind*: PolyTypeKind
    #     of Forall:
    #         gen*: HashSet[TypeVar]
    #         typ*: ref Value
    #     of PolyTypeKind.Intersection:
    #         types*: seq[PolyType]   # TODO: ref irukana? kanngaete

    SymbolKind* {.pure.} = enum
        Var
        Let
        Const
        Typ
        Func

    SymbolId = int
    Symbol* = ref object
        id*: SymbolId
        kind*: SymbolKind
        global*: bool
        decl*: Ident   # assume be in IdentDef or Function
        # ptyp*: PolyType
        typ*: ref Value
        impl*: Term
        use*: seq[Term]
        instances*: Table[ref Value, Impl]
    Impl* = ref object
        instance*: Option[Term]
        lty*: llvm.Type
        val*: llvm.Value

proc Literal*(_: typedesc[TermKind]): set[TermKind] =
    {TermKind.`()`..TermKind.String}

suite IdentDef:
    proc `$`*(self: ref Value): string
    proc `$`*(self: Term): string
    proc `$`*(self: IdentDef): string =
        result = $self.pat
        if self.typ.isSome:
            result.add fmt": {self.typ.get}"
        elif self.pat.typ.kind != ValueKind.Unit:
            result.add fmt"(: {self.pat.typ})"
        if self.default.isSome:
            result.add fmt" = {self.default.get}"
    proc newIdentDef*(pat: Term): IdentDef =
        IdentDef(pat: pat, typ: none(Term), default: none(Term))
    proc newIdentDef*(pat: Term, typ: Term): IdentDef =
        IdentDef(pat: pat, typ: some(typ), default: none(Term))
    proc newIdentDef*(pat: Term, default: Term): IdentDef =
        IdentDef(pat: pat, typ: none(Term), default: some(default))
    proc newIdentDef*(pat: Term, typ: Term, default: Term): IdentDef =
        IdentDef(pat: pat, typ: some(typ), default: some(default))

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
        let param = if self.param.len == 0: "" else: ": " & self.param.map(`$`).join", "
        fmt"![{name}{param}]"
    proc Link*(_: typedesc[Metadata], param: seq[Term]): Metadata =
        Metadata(kind: MetadataKind.Link, param: param)
    proc ImportLL*(_: typedesc[Metadata], param: seq[Term]): Metadata =
        Metadata(kind: MetadataKind.ImportLL, param: param)
    proc Subtype*(_: typedesc[Metadata]): Metadata =
        Metadata(kind: MetadataKind.Subtype)
    proc Userdef*(_: typedesc[Metadata], name: string, param: seq[Term]): Metadata =
        Metadata(kind: MetadataKind.Userdef, param: param, name: name)

suite Body:
    proc `$`*(self: Body): string =
        $self.term
    proc newBody*(term: Term, scope: Scope): Body =
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
    proc newFunction*(id: Ident, paramty: IdentDefs, rety: Term, body: Body, metadata: Option[Metadata] = none Metadata): Function =
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


suite Value:
    proc hash*(self: TypeVar): Hash =
        result = !$ self.id
    proc hash*(self: GenType): Hash =
        result = !$ self.name.hash
    proc hash*(self: ref Value): Hash =
        result = 0
        result = result !& self.kind.ord
        # TODO: implement hash of Value
        # case self.kind:
        # of ValueKind.Bottom:
        #     discard
        # of ValueKind.Top:
        #     discard
        # of ValueKind.Unit:
        #     discard
        # of ValueKind.Bool:
        #     discard
        # of ValueKind.Integer:
        #     discard
        # of ValueKind.Float:
        #     discard
        # of ValueKind.Char:
        #     discard
        # of ValueKind.String:
        #     discard
        # of ValueKind.List:
        #     discard
        # of ValueKind.Tuple:
        #     discard
        # of ValueKind.Record:
        #     discard
        # of ValueKind.Arrow:
        #     discard
        # of ValueKind.Typedesc:
        #     discard
        # of ValueKind.Var:
        #     discard
        # of ValueKind.Intersection:
        #     discard
        result = !$result
    proc `==`*(self, other: ref Value): bool
    proc `==`*(self, other: Value): bool =
        ## equality as nim object
        if self.kind == ValueKind.Link:
            self.to[] == other
        elif other.kind == ValueKind.Link:
            self == other.to[]
        elif self.kind == other.kind:
            case self.kind
            # of Bottom, Top, ValueKind.Unit, ValueKind.Bool, ValueKind.Integer, ValueKind.Float, ValueKind.Char, ValueKind.String:
            of Bottom, ValueKind.`()`, ValueKind.Unit, ValueKind.Bool, ValueKind.Float, ValueKind.Char, ValueKind.String:
                true
            of ValueKind.Integer:
                self.bits == other.bits
            of ValueKind.U:
                self.level == other.level
            # of ValueKind.List:
            #     self.elem == other.elem
            of ValueKind.Ptr:
                self.pointee == other.pointee
            of ValueKind.Pair:
                self.first == other.first and self.second == other.second
            of ValueKind.Record:
                if self.members.len == other.members.len:
                    var ret = true
                    for member in self.members.keys:
                        if member notin other.members:
                            ret = false
                        else:
                            ret = ret and self.members[member] == other.members[member]
                    ret
                else:
                    false
            # of ValueKind.Tuple, ValueKind.Intersection:
            of ValueKind.Intersection, ValueKind.Union:
                self.types == other.types
            # of ValueKind.Record:
            #     self.idtypes.zip(other.idtypes).mapIt(it[0] == it[1]).foldl(a and b)
            # of Arrow:
            #     self.paramty.zip(other.paramty).mapIt(it[0] == it[1]).foldl(a and b) and
            #     self.rety == other.rety
            of ValueKind.Pi:
                self.genty.zip(other.genty).mapIt(it[0][0] == it[1][0] and it[0][1] == it[1][1]).foldl(a and b, true) and
                self.paramty.zip(other.paramty).mapIt(it[0] == it[1]).foldl(a and b, true) and
                self.rety == other.rety
            # of ValueKind.Sigma:
            #     self.first == other.first and
            #     self.second == other.second
            of Typedesc:
                self.`typedesc` == other.`typedesc`
            # of Distinct:
            #     self.base == other.base and self.id == other.id
            of ValueKind.Var:
                self.tv == other.tv
            of ValueKind.Link:
                self.to == other.to
            of ValueKind.Gen:
                self.gen.name == other.gen.name
            of ValueKind.BoolV:
                self.boolval == other.boolval
            of ValueKind.IntV:
                self.intval == other.intval
            of ValueKind.FloatV:
                self.floatval == other.floatval
            of ValueKind.CharV:
                self.charval == other.charval
            of ValueKind.StringV:
                self.strval == other.strval

        else:
            false
    proc `==`*(self, other: ref Value): bool =
        ## equality as nim object
        self[] == other[]
    proc `$`*(self: Value): string =
        case self.kind
        of ValueKind.Bottom:
            "Bottom"
        # of ValueKind.Top:
        #     "Top"
        of ValueKind.`()`:
            "()"
        of ValueKind.Unit:
            "unit"
        of ValueKind.U:
            if self.level == 0:
                "Type"
            else:
                "Type" & $self.level
        of ValueKind.Bool:
            # $"bool".red
            "bool"
        of ValueKind.Integer:
            # $"int".red
            if self.bits == wordSize():
                "int"
            else:
                fmt"int{self.bits}"
        of ValueKind.Float:
            # $"float".red
            "float"
        of ValueKind.Char:
            # $"char".red
            "char"
        of ValueKind.String:
            # $"string".red
            "string"
        # of ValueKind.List:
        #     fmt"[{self.base[]}]"
        of ValueKind.Ptr:
            fmt"Ptr[{self.pointee}]"
        of ValueKind.Pair:
            fmt"({self.first}, {self.second})"
        of ValueKind.Record:
            let s = toSeq(self.members.pairs).mapIt(fmt"{it[0]}: {it[1]}").join", "
            fmt"({s})"
        # of ValueKind.Tuple:
        #     "(" & self.types.mapIt($it[]).join(", ") & ")"
        # of ValueKind.Record:
        #     "{" & self.idtypes.mapIt(fmt"{it[0]}: {it[1][]}").join(", ") & "}"
        # of ValueKind.Arrow:
        #     self.paramty.mapIt($it[]).join(", ") & " -> " & $self.rety[]
        of ValueKind.Pi:
            (
                if self.genty.len == 0:
                    ""
                else:
                    self.genty.mapIt(fmt"∀{it[1]}").join(", ") & " "
            ) &
            (
                if self.paramty.len == 0:
                    "()"
                else:
                    "(" & self.paramty.mapIt($it[]).join(", ") & ")"
            ) & " -> " & $self.rety[]
        # of ValueKind.Sigma:
        #     fmt"({self.first}, {self.second})"
        of ValueKind.Typedesc:
            # let t = "typedesc".red
            let t = "typedesc"
            fmt"{t}[{self.`typedesc`[]}]"
        of ValueKind.Var:
            $self.tv
        of ValueKind.Intersection:
            self.types.mapIt(
                # if it.kind == ValueKind.Arrow:
                #     fmt"({it[]})"
                if it.kind == ValueKind.Pi:
                    fmt"({it[]})"
                else:
                    $it[]
            ).join("^")
        of ValueKind.Union:
            self.types.mapIt(
                # if it.kind == ValueKind.Arrow:
                #     fmt"({it[]})"
                if it.kind == ValueKind.Pi:
                    fmt"({it[]})"
                else:
                    $it[]
            ).join("\\/")
        # of ValueKind.Distinct:
        #     fmt"distinct {self.base[]}"
        of ValueKind.Link:
            $self.to
        of ValueKind.Gen:
            fmt"{self.gen.name}: {self.gen.typ} (<: {self.gen.ub})"
        of ValueKind.BoolV:
            fmt"{self.boolval}"
        of ValueKind.IntV:
            fmt"{self.intval}"
        of ValueKind.FloatV:
            fmt"{self.floatval}"
        of ValueKind.CharV:
            fmt"{self.charval}"
        of ValueKind.StringV:
            fmt"{self.strval}"
    proc `$`*(self: ref Value): string = $self[]
    var
        tvid: TypeVarId = -1
        dtid: DistinctTypeId = -1

    proc Bottom*(_: typedesc[Value]): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Bottom)
    # proc Top*(_: typedesc[Value]): ref Value =
    #     result = new Value
    #     result[] = Value(kind: ValueKind.Top)
    proc unit*(_: typedesc[Value]): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.`()`)
    proc Unit*(_: typedesc[Value]): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Unit)
    proc newTypeVar*(): TypeVar =
        inc tvid
        TypeVar(
            id: tvid,
            # ub: Value.Top,
            ub: Value.Unit,
            lb: Value.Bottom
        )
    proc U*(_: typedesc[Value], level: int = 0): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.U, level: level)
    proc Bool*(_: typedesc[Value]): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Bool)
    proc Integer*(_: typedesc[Value], bits: uint): ref Value =
        result = new Value
        let bits = if bits == 0: wordSize() else: bits
        result[] = Value(kind: ValueKind.Integer, bits: bits)
    proc Float*(_: typedesc[Value]): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Float)
    proc Char*(_: typedesc[Value]): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Char)
    proc String*(_: typedesc[Value]): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.String)
    proc List*(_: typedesc[Value], elem: ref Value): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.List, elem: elem)
    proc Ptr*(_: typedesc[Value], pointee: ref Value): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Ptr, pointee: pointee)
    proc Pair*(_: typedesc[Value], first, second: ref Value): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Pair, first: first, second: second)
    proc Sigma*(_: typedesc[Value], first, second: ref Value): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Sigma, first: first, second: second)
    # proc Tuple*(_: typedesc[Value], types: seq[ref Value]): ref Value =
    #     result = new Value
    #     result[] = Value(kind: ValueKind.Tuple, types: types)
    proc Record*(_: typedesc[Value], members: seq[(string, ref Value)]): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Record, members: members.toTable)
    proc Record*(_: typedesc[Value], members: Table[string, ref Value]): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Record, members: members)
    proc Arrow*(_: typedesc[Value], params: seq[ref Value], rety: ref Value): ref Value =
        result = new Value
        # result[] = Value(kind: ValueKind.Arrow, paramty: params, rety: rety)
        result[] = Value(kind: ValueKind.Pi, genty: @[], paramty: params, rety: rety)
    proc Pi*(_: typedesc[Value], genty: seq[(Term, ref Value)], params: seq[ref Value], rety: ref Value): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Pi, genty: genty, paramty: params, rety: rety)
    proc Typedesc*(_: typedesc[Value], typ: ref Value): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Typedesc, `typedesc`: typ)
    proc Distinct*(_: typedesc[Value], base: ref Value): ref Value =
        inc dtid
        result = new Value
        result[] = Value(kind: ValueKind.Intersection, base: base, id: dtid)
    # proc Var*(_: typedesc[Value]): ref Value =
    #     result = new Value
    #     result[] = Value(kind: ValueKind.Var, tv: newTypeVar())
    proc Intersection*(_: typedesc[Value], types: seq[ref Value]): ref Value =
        result = case types.len
        of 0:
            assert false, ""
            Value.Unit
        of 1:
            types[0]
        else:
            var tmp = new Value
            tmp[] = Value(kind: ValueKind.Intersection, types: types.toHashSet)
            tmp
    proc Intersection*(_: typedesc[Value], types: HashSet[ref Value]): ref Value =
        result = case types.len
        of 0:
            assert false, ""
            Value.Unit
        of 1:
            toSeq(types.items)[0]
        else:
            var tmp = new Value
            tmp[] = Value(kind: ValueKind.Intersection, types: types)
            tmp
    proc Union*(_: typedesc[Value], types: seq[ref Value]): ref Value =
        result = case types.len
        of 0:
            assert false, ""
            Value.Bottom
        of 1:
            types[0]
        else:
            var tmp = new Value
            tmp[] = Value(kind: ValueKind.Union, types: types.toHashSet)
            tmp
    proc Union*(_: typedesc[Value], types: HashSet[ref Value]): ref Value =
        result = case types.len
        of 0:
            assert false, ""
            Value.Bottom
        of 1:
            toSeq(types.items)[0]
        else:
            var tmp = new Value
            tmp[] = Value(kind: ValueKind.Union, types: types)
            tmp
    proc Link*(_: typedesc[Value], to: ref Value): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Link, to: to)
    proc Gen*(_: typedesc[Value], name: string, ub: ref Value, typ: ref Value): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.Gen, gen: GenType(name: name, ub: ub, typ: typ))

    proc BoolV*(_: typedesc[Value], boolval: bool): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.BoolV, boolval: boolval)
    proc IntV*(_: typedesc[Value], intval: BiggestInt): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.IntV, intval: intval)
    proc FloatV*(_: typedesc[Value], floatval: BiggestFloat): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.FloatV, floatval: floatval)
    proc CharV*(_: typedesc[Value], charval: char): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.CharV, charval: charval)
    proc StringV*(_: typedesc[Value], strval: string): ref Value =
        result = new Value
        result[] = Value(kind: ValueKind.StringV, strval: strval)

    proc hasRegion*(self: ref Value): bool =
        case self.kind
        of ValueKind.Bottom..ValueKind.Char:
            false
        of ValueKind.String:
            true
        # of ValueKind.Arrow:
        #     true
        # of ValueKind.List:
        #     true
        of ValueKind.Ptr:
            true
        of ValueKind.Pair:
            self.first.hasRegion or self.second.hasRegion
        of ValueKind.Record:
            toSeq(self.members.values).any(hasRegion)
        # of ValueKind.Tuple, ValueKind.Intersection:
        of ValueKind.Pi:
            true
        # of ValueKind.Sigma:
        #     self.first.hasRegion or self.second.hasRegion
        of ValueKind.Intersection, ValueKind.Union:
            toSeq(self.types.items).any(hasRegion)
        # of ValueKind.Record:
        #     self.idtypes.anyIt(it[1].hasRegion)
        # of ValueKind.Distinct, ValueKind.Link:
        of ValueKind.Link:
            self.`typedesc`.hasRegion
        of ValueKind.Var, ValueKind.TypeDesc:
            assert false
            false
        of ValueKind.Gen:
            assert false, ""
            false
        of ValueKind.BoolV..ValueKind.StringV:
            false

    proc compilable*(self: ref Value): bool =
        case self.kind
        of ValueKind.Bottom:
            false
        of ValueKind.`()`..ValueKind.String:
            true
        # of ValueKind.Arrow:
        #     self.paramty.all(compilable) and self.rety.compilable
        # of ValueKind.List:
        #     self.elem.compilable
        of ValueKind.Ptr:
            self.pointee.compilable
        of ValueKind.Pair:
            self.first.compilable and self.second.compilable
        of ValueKind.Record:
            toSeq(self.members.values).all(compilable)
        # of ValueKind.Tuple:
        #     self.types.all(compilable)
        of ValueKind.Pi:
            self.paramty.all(compilable) and self.rety.compilable
        # of ValueKind.Sigma:
        #     self.first.compilable and self.second.compilable
        of ValueKind.Intersection, ValueKind.Union:
            false
        # of ValueKind.Record:
        #     self.idtypes.anyIt(it[1].compilable)
        # of ValueKind.Distinct, ValueKind.Link:
        of ValueKind.Link:
            self.`typedesc`.compilable
        of ValueKind.Var, ValueKind.TypeDesc:
            assert false
            false
        of ValueKind.Gen:
            false
        of ValueKind.BoolV..ValueKind.StringV:
            true


    proc typ*(self: ref Value): ref Value =
        case self.kind
        of ValueKind.Bottom:
            Value.U
        # of ValueKind.Top # unit Type is Top?:
        #     nil
        of ValueKind.`()`:
            Value.Unit
        of ValueKind.Unit:
            Value.U
        of ValueKind.U:
            Value.U(self.level + 1)
        # of ValueKind.Bool:
        #     nil
        of ValueKind.Bool:
            Value.U
        of ValueKind.Integer:
            Value.U
        of ValueKind.Float:
            Value.U
        of ValueKind.Char:
            Value.U
        of ValueKind.String:
            Value.U
        # of ValueKind.List:
        #     nil
        of ValueKind.Ptr:
            self.pointee.typ
        of ValueKind.Pair:
            let
                ft = self.first.typ
                st = self.second.typ
            if ft.kind == ValueKind.U and st.kind == ValueKind.U:
                Value.U(max(ft.level, st.level))
            else:
                Value.Pair(ft, st)
        # of ValueKind.Tuple:
        #     nil
        # TODO: implement below
        of ValueKind.Record:
            Value.U
        # of ValueKind.Arrow:
        #     nil
        of ValueKind.Pi:
            nil
        # of ValueKind.Sigma:
        #     nil
        of ValueKind.Typedesc:
            self.`typedesc`.typ
        # of ValueKind.Distinct:
        #     nil
        of ValueKind.Var:
            nil
        of ValueKind.Intersection, ValueKind.Union:
            nil
        of ValueKind.Link:
            self.to.typ
        of ValueKind.Gen:
            self.gen.typ
        of ValueKind.BoolV:
            Value.Bool
        of ValueKind.IntV:
            Value.Integer(0)
        of ValueKind.FloatV:
            Value.Float
        of ValueKind.CharV:
            Value.Char
        of ValueKind.StringV:
            Value.String

# suite PolyType:
#     proc Forall*(_: typedesc[PolyType], gen: HashSet[TypeVar], typ: ref Value): PolyType =
#         PolyType(kind: PolyTypeKind.Forall, gen: gen, typ: typ)
#     proc Intersection*(_: typedesc[PolyType], gen: HashSet[TypeVar], types: seq[PolyType]): PolyType =
#         PolyType(kind: PolyTypeKind.Intersection, types: types)
#     proc `$`*(self: PolyType): string =
#         case self.kind
#         of PolyTypeKind.ForAll:
#             let tmp = if self.gen.len == 0: "" else: "∀" & toSeq(self.gen.items).map(`$`).join(".∀") & "."
#             fmt"{tmp}{self.typ}"
#         of PolyTypeKind.Intersection:
#             self.types.join("∧")

suite Term:
    proc `$`*(self: Term): string =
        case self.kind
        of TermKind.Failed:
            "failed term"
        of TermKind.bottom:
            "⊥"
        of TermKind.`()`:
            "()"
        of TermKind.Unit:
            "unit"
        of TermKind.U:
            "Type"
        of TermKind.Bool:
            $self.boolval
        of TermKind.Integer:
            if self.bits == wordSize():
                $self.intval
            else:
                $self.intval & fmt"'i{self.bits}"
        of TermKind.Float:
            $self.floatval
        of TermKind.Char:
            $self.charval
        of TermKind.String:
            "\"" & self.strval & "\""
        of TermKind.Id:
            $self.name
        # of TermKind.Lambda:
        #     fmt"λ{self.param}.{self.body}"
        # of TermKind.List:
        #     let s = self.terms.map(`$`).join(", ")
        #     fmt"[{s}]"
        of TermKind.Tuple:
            let s = self.terms.map(`$`).join(", ")
            fmt"({s})"
        of TermKind.Record:
            let s = toSeq(self.members.pairs).mapIt(fmt"{it[0]}: {it[1]}").join(", ")
            fmt"({s})"
        of TermKind.Let:
            &"let {self.iddef}"
        of TermKind.Var:
            fmt"var {self.iddef}"
        of TermKind.Const:
            &"const {self.iddef}"
        # of TermKind.Typedef:
        #     let s = self.typedefs.map(`$`).join("\n")
        #     &"type\n{s.indent(2)}"
        of TermKind.Funcdef, TermKind.FuncdefInst:
            $self.fn
        of TermKind.FunctionInst:
            let s = self.instargs.map(`$`).join(", ")
            fmt"{self.pfn}[{s}]"
        of TermKind.If:
            let
                elift = self.`elif`.mapIt(&"elif {it[0]}:\n{($it[1]).indent(2)}").join("\n")[2..^1]
                elset = ($self.`else`).indent(2)
            &"{elift}\nelse:\n{elset}"
        # of TermKind.When:
        #     ""
        of TermKind.Case:
            ""
        # of TermKind.While:
        #     ""
        # of TermKind.For:
        #     ""
        of TermKind.Loop:
            let s = $self.`block`
            &"loop {self.label}\n{s.indent(2)}"
        of TermKind.Block:
            let s = $self.`block`
            &"block {self.label}\n{s.indent(2)}"
        of TermKind.Asign:
            fmt"{self.pat} = {self.val}"
        of TermKind.Typeof:
            fmt"typeof({self.term})"
        of TermKind.Malloc:
            fmt"malloc({self.malloctype}, {self.mallocsize})"
        of TermKind.Discard:
            fmt"discard {self.term}"
        of TermKind.Apply:
            let
                args = self.args.join(", ")
            fmt"{self.callee}({args})"
        # of TermKind.Projection:
        #     fmt"{self.container}.{self.index}"
        of TermKind.Meta:
            fmt"{self.metadata}"
        of TermKind.Seq:
            let f = proc(it: Term): string =
                let tmp = if it.typ.isNil or it.typ.kind == ValueKind.Unit:
                    ""
                else:
                    # if it.typ.region.isNil:
                    #     fmt" (: {it.typ})"
                    # else:
                    #     fmt" (: {it.typ}{it.typ.region})"
                    fmt" (: {it.typ})"
                fmt"{it}{tmp}"
            self.terms.map(f).join("\n")
        of TermKind.Us:
            "_"

    proc Failed*(_: typedesc[Term]): Term =
        Term(kind: TermKind.Failed)
    proc bottom*(_: typedesc[Term]): Term =
        Term(kind: TermKind.bottom)
    proc unit*(_: typedesc[Term]): Term =
        Term(kind: TermKind.`()`)
    proc Unit*(_: typedesc[Term]): Term =
        Term(kind: TermKind.Unit)
    proc U*(_: typedesc[Term], level: int = 0): Term =
        Term(kind: TermKind.U, level: level)
    proc Bool*(_: typedesc[Term], boolval: bool): Term =
        Term(kind: TermKind.Bool, boolval: boolval)
    proc Integer*(_: typedesc[Term], intval: BiggestInt, bits: uint): Term =
        if bits == 0:
            Term(kind: TermKind.Integer, intval: intval, bits: wordSize())
        else:
            Term(kind: TermKind.Integer, intval: intval, bits: bits)
    proc Float*(_: typedesc[Term], floatval: float): Term =
        Term(kind: TermKind.Float, floatval: floatval)
    proc Char*(_: typedesc[Term], charval: char): Term =
        Term(kind: TermKind.Char, charval: charval)
    proc String*(_: typedesc[Term], strval: string): Term =
        Term(kind: TermKind.String, strval: strval)
    proc Id*(_: typedesc[Term], name: string): Term =
        Term(kind: TermKind.Id, name: name)
    proc Lambda*(_: typedesc[Term], param: IdentDefs, body: Body): Term =
        Term(kind: TermKind.Lambda, param: param, body: body)
    proc List*(_: typedesc[Term]): Term =
        Term()
    proc Tuple*(_: typedesc[Term], terms: seq[Term]): Term =
        Term(kind: TermKind.Tuple, terms: terms)
    proc Record*(_: typedesc[Term], members: Table[string, Term]): Term =
        Term(kind: TermKind.Record, members: members)
    proc Let*(_: typedesc[Term], iddef: IdentDef): Term =
        Term(kind: TermKind.Let, iddef: iddef)
    proc Var*(_: typedesc[Term], iddef: IdentDef): Term =
        Term(kind: TermKind.Var, iddef: iddef)
    proc Const*(_: typedesc[Term], iddef: IdentDef): Term =
        Term(kind: TermKind.Const, iddef: iddef)
    proc Typedef*(_: typedesc[Term], iddefs: IdentDefs): Term =
        Term(kind: TermKind.Typedef, typedefs: iddefs)
    proc Funcdef*(_: typedesc[Term], fn: Function): Term =
        Term(kind: TermKind.Funcdef, fn: fn)
    proc FunctionInst*(_: typedesc[Term], pfn: Term, instargs: seq[Term]): Term =
        Term(kind: TermKind.FunctionInst, pfn: pfn, instargs: instargs)
    proc If*(_: typedesc[Term], `elif`: seq[(Term, Body)], `else`: Body): Term =
        Term(kind: TermKind.If, `elif`: `elif`, `else`: `else`)
    proc When*(_: typedesc[Term]): Term =
        Term()
    proc Case*(_: typedesc[Term]): Term =
        Term()
    proc While*(_: typedesc[Term]): Term =
        Term()
    proc For*(_: typedesc[Term]): Term =
        Term()
    proc Loop*(_: typedesc[Term], label: Ident, `block`: Body): Term =
        Term(kind: TermKind.Loop, label: label, `block`: `block`)
    proc Block*(_: typedesc[Term], label: Ident, `block`: Body): Term =
        Term(kind: TermKind.Block, label: label, `block`: `block`)
    proc Asign*(_: typedesc[Term], pat: Term, val: Term): Term =
        Term(kind: TermKind.Asign, pat: pat, val: val)
    proc Typeof*(_: typedesc[Term], term: Term): Term =
        Term(kind: TermKind.Typeof, term: term)
    proc Malloc*(_: typedesc[Term], malloctype: Term, mallocsize: Term): Term =
        Term(kind: TermKind.Malloc, malloctype: malloctype, mallocsize: mallocsize)
    proc Discard*(_: typedesc[Term], term: Term): Term =
        Term(kind: TermKind.Discard, term: term)
    proc Apply*(_: typedesc[Term], callee: Term, args: seq[Term]): Term =
        Term(kind: TermKind.Apply, callee: callee, args: args)
    # proc Projection*(_: typedesc[Term], container: Term, index: range[0..1]): Term =
    #     result = new Term
    #     result[] = Term(kind: TermKind.Projection, container: container, index: index)
    proc Meta*(_: typedesc[Term], meta: Metadata): Term =
        Term(kind: TermKind.Meta, metadata: meta)
    proc Seq*(_: typedesc[Term], terms: seq[Term]): Term =
        Term(kind: TermKind.Seq, terms: terms)
    proc Us*(_: typedesc[Term]): Term =
        Term(kind: TermKind.Us)

    proc newIdent*(name: string): Ident =
        Term.Id(name)

suite Symbol:
    proc `$`*(self: Symbol): string =
        let
            kind = if self.kind == SymbolKind.Typ:
                "Type"
            else:
                $self.kind
            id = if self.global:
                $self.decl
            else:
                $self.decl
            typ = $self.typ
            impl = if self.kind == SymbolKind.Func:
                "..."
            else:
                $self.impl
            loc = self.decl.loc
        fmt"{loc}: ({kind}){id}: {typ} (= {impl})"
    var
        symid = 0

    proc Let*(_: typedesc[Symbol], ident: Ident, typ: ref Value, impl: Term, global: bool = false): Symbol =
        inc symid
        result = Symbol(
            kind: SymbolKind.Let, id: symid,
            decl: ident, typ: typ, impl: impl,
            global: global, use: @[]
        )
        result.typ.symbol = some result
    proc Func*(_: typedesc[Symbol], ident: Ident, typ: ref Value, impl: Term, global: bool = false): Symbol =
        inc symid
        result = Symbol(
            kind: SymbolKind.Func, id: symid,
            decl: ident, typ: typ, impl: impl,
            global: global, use: @[]
        )
        result.typ.symbol = some result
    proc Typ*(_: typedesc[Symbol], ident: Ident, typ: ref Value, impl: Term, global: bool = false): Symbol =
        inc symid
        result = Symbol(
            kind: SymbolKind.Typ, id: symid,
            decl: ident, typ: typ, impl: impl,
            global: global, use: @[]
        )
        result.typ.symbol = some result
    proc Const*(_: typedesc[Symbol], ident: Ident, typ: ref Value, impl: Term, global: bool = false): Symbol =
        inc symid
        result = Symbol(
            kind: SymbolKind.Const, id: symid,
            decl: ident, typ: typ, impl: impl,
            global: global, use: @[]
        )
        result.typ.symbol = some result

# suite Symbol:
#     proc `$`*(self: Symbol): string =
#         $self[]

suite Impl:
    proc `$`*(self: Impl): string = $self[]

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

    proc newScope*(parent: Scope = nil): Scope =
        Scope(
            parent: parent,
            syms: initTable[string, seq[Symbol]](),
            typeOrder: if parent.isNil: newOrder[ref Value]() else: parent.typeOrder,
            converters: initTable[(ref Value, ref Value), Ident]()
        )
