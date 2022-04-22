
import tables
import sets
import options

import ../lineinfos
import ../orders
from llvm import nil


type
    Program* = ref object
        stmts*: seq[Statement]
        scope*: Scope
    StatementKind* {.pure.} = enum
        For
        While
        Loop
        LetSection
        VarSection
        ConstSection
        TypeSection
        Asign
        Funcdef
        Meta
        Discard
        Comments
        Expression
        Fail
    Statement* = ref StatementObject
    StatementObject = object
        loc*: Location
        case kind*: StatementKind
        of StatementKind.Loop:
            label*: Option[Ident]
            `block`*: Suite
        of StatementKind.While:
            branch*: ElifBranch
        of StatementKind.LetSection, StatementKind.VarSection, StatementKind.ConstSection:
            iddefs*: seq[IdentDef]
        of StatementKind.TypeSection:
            typedefs*: seq[TypeDef]
        of StatementKind.For, StatementKind.Asign:
            pat*: Pattern
            val*: Expression
            op*: Ident
            suite*: Suite # for `for`
        of StatementKind.Funcdef:
            fn*: Function
        of StatementKind.Meta:
            meta*: Metadata
        of StatementKind.Discard:
            `discard`*: Option[Expression]
        of StatementKind.Comments:
            comments*: seq[string]
        of StatementKind.Expression:
            expression*: Expression
        of StatementKind.Fail:
            nil
    Suite* = ref object
        stmts*: seq[Statement]
        scope*: Scope
    ElifBranch* = tuple[cond: Expression, suite: Suite]
    ElseBranch* = Suite
    OfBranch* = tuple[pat: Pattern, suite: Suite]
    Scope* = ref object
        parent*: Scope
        syms*: Table[string, seq[Symbol]]
        consts*: Table[string, seq[Symbol]]
        typeOrder*: Order[Value]  # cumulative
        converters*: Table[(Value, Value), Ident]
    IdentDef* = ref object
        pat*: Pattern
        typ*: Option[Expression]
        default*: Option[Expression]
    TypeDef* = ref object
        id*: Ident
        params*: Option[seq[GenTypeDef]]
        typ*: TypeExpression
    GenTypeDef* = ref object
        id*: Ident
        ub*: Option[Expression]
    FunctionParam* = ref object
        implicit*: seq[GenTypeDef]
        params*: seq[IdentDef]
        rety*: Option[Expression]
        scope*: Scope
    Function* = ref object
        isProp*: bool
        id*: Ident
        param*: FunctionParam
        metadata*: Option[Metadata]
        suite*: Option[Suite]
    ExpressionKind* {.pure.} = enum
        Literal
        Ident
        Tuple
        Array
        Record
        If
        When
        Case
        Call
        Command
        Dot
        Bracket
        Binary
        Prefix
        Postfix
        Block
        Lambda
        Malloc
        Typeof
        Ref
        FnType
        Fail
    Expression* = ref ExpressionObject
    ExpressionObject = object
        loc*: Location
        typ*: Value
        case kind*: ExpressionKind
        of ExpressionKind.Literal:
            litval*: Literal
        of ExpressionKind.Ident:
            ident*: Ident
        of ExpressionKind.Tuple, ExpressionKind.Array:
            exprs*: seq[Expression]
        of ExpressionKind.Record:
            members*: seq[(Ident, Expression)]
        of ExpressionKind.If, ExpressionKind.When:
            elifs*: seq[ElifBranch]
            elseb*: Option[ElseBranch]
        of ExpressionKind.Case:
            val*: Expression
            ofs*: seq[OfBranch]
            default*: Option[ElseBranch]
        of ExpressionKind.Call, ExpressionKind.Command, ExpressionKind.Bracket, ExpressionKind.FnType:
            callee*: Expression
            args*: seq[Expression]
            rety*: Expression
        of ExpressionKind.Dot, ExpressionKind.Binary, ExpressionKind.Prefix, ExpressionKind.Postfix:
            lhs*: Expression
            rhs*: Expression
            op*: Ident # for binary and unary
            expression*: Expression # for unary
        of ExpressionKind.Block:
            label*: Option[Ident]
            `block`*: Suite
        of ExpressionKind.Lambda:
            param*: FunctionParam
            body*: Suite
        of ExpressionKind.Malloc:
            mtype*: Expression
            msize*: Expression
        of ExpressionKind.Typeof:
            `typeof`*: Expression
        of ExpressionKind.Ref:
            `ref`*: Expression
        of ExpressionKind.Fail:
            nil
    TypeExpressionKind* {.pure.} = enum
        Object
        Sum
        Distinct
        Trait
        Expression
    TypeExpression* = ref TypeExpressionObject
    TypeExpressionObject = object
        loc*: Location
        isRef*: bool
        case kind*: TypeExpressionKind
        of TypeExpressionKind.Object:
            members*: seq[(Ident, TypeExpression)]
        of TypeExpressionKind.Sum:
            sum*: SumType
        of TypeExpressionKind.Distinct:
            base*: TypeExpression
        of TypeExpressionKind.Trait:
            trait*: TraitType
        of TypeExpressionKind.Expression:
            expression*: Expression
    SumType* = object
        constructors*: seq[SumConstructor]
    SumConstructorKind* {.pure.} = enum
        NoField
        UnnamedField
        NamedField
    SumConstructor* = ref SumConstructorObject
    SumConstructorObject = object
        id*: Ident
        case kind*: SumConstructorKind
        of SumConstructorKind.NoField:
            nil
        of SumConstructorKind.UnnamedField:
            types*: seq[Expression]
        of SumConstructorKind.NamedField:
            fields*: seq[(Ident, Expression)]
    TraitType* = object
        pat*: Pattern
        typ*: Expression
        traits*: seq[Trait]
    Trait* = ref TraitObject
    TraitKind* = enum
        Is
        Func
    TraitObject = object
        case kind*: TraitKind
        of TraitKind.Is:
            pat*: Pattern
            val*: TypeExpression
        of TraitKind.Func:
            fn*: Function

    LiteralKind* {.pure.} = enum
        unit
        bool
        integer
        float
        char
        string
        Univ
    Literal* = ref LiteralObject
    LiteralObject = object
        case kind*: LiteralKind
        of LiteralKind.unit:
            nil
        of LiteralKind.bool:
            boolval*: bool
        of LiteralKind.integer:
            intval*: BiggestInt
            intbits*: uint
        of LiteralKind.float:
            floatval*: BiggestFloat
            floatbits*: uint
        of LiteralKind.char:
            charval*: char
        of LiteralKind.string:
            strval*: string
        of LiteralKind.Univ:
            level*: uint
    Ident* = ref object
        name*: string
        loc*: Location
        typ*: Value
    PatternKind* {.pure.} = enum
        Literal
        Ident
        Dot
        # Bracket
        Tuple
        Record
        UnderScore
    Pattern* = ref PatternObject
    PatternObject = object
        loc*: Location
        typ*: Value
        case kind*: PatternKind
        of PatternKind.Literal:
            litval*: Literal
        of PatternKind.Ident:
            ident*: Ident
            index*: Option[Expression]
        of PatternKind.Dot:
            lhs*: Pattern
            rhs*: Ident
        # of PatternKind.Bracket:
        #     callee*: Pattern
        #     args*: seq[Expression]
        of PatternKind.Tuple, PatternKind.Record:
            tag*: Option[Ident]
            patterns*: seq[Pattern] # for Tuple
            members*: seq[(Ident, Pattern)] # for Record
        of PatternKind.UnderScore:
            nil
    MetadataKind* {.pure.} = enum
        Link
        ImportLL
        Subtype
        Userdef
    Metadata* = object
        case kind*: MetadataKind
        of MetadataKind.Link..MetadataKind.Subtype:
            nil
        of MetadataKind.Userdef:
            name*: string
        params*: seq[Expression]
    ValueKind* {.pure.} = enum
        Literal
        Bottom
        Unit
        Bool
        Integer
        Float
        Char
        String
        Pair
        Array
        ArrayV
        Record
        Ptr
        Pi
        Sum
        Trait
        Singleton
        Distinct
        Intersection
        Union
        Cons
        Var
        Gen
        Link
    Value* = ref ValueObject
    ValueObject = object
        symbol*: Option[Symbol]
        region*: Region
        case kind*: ValueKind
        of ValueKind.Literal:
            litval*: Literal
        of ValueKind.Bottom, ValueKind.Unit, ValueKind.Bool:
            nil
        of ValueKind.Integer, ValueKind.Float:
            bits*: uint
        of ValueKind.Char, ValueKind.String:
            nil
        of ValueKind.Pair:
            first*: Value
            second*: Value
        of ValueKind.Array, ValueKind.Singleton, ValueKind.Distinct:
            base*: Value
        of ValueKind.ArrayV:
            vals*: seq[Value]
        of ValueKind.Record:
            members*: Table[Ident, Value]
        of ValueKind.Ptr:
            pointee*: Value
        of ValueKind.Pi, ValueKind.Cons:
            implicit*: seq[GenericType]
            params*: seq[Value] # not concerned with `Cons`
            rety*: Value
        of ValueKind.Sum:
            cons*: Table[Ident, Value]
        of ValueKind.Trait:
            paty*: (Pattern, Value)
            iss*: seq[(Pattern, Value)]
            fns*: seq[Function]
        of ValueKind.Intersection, ValueKind.Union:
            types*: HashSet[Value]
        of ValueKind.Var:
            tv*: TypeVar
        of ValueKind.Gen:
            gt*: GenericType
        of ValueKind.Link:
            to*: Value
    TypeVarId = int
    TypeVar* = object
        id*: TypeVarId
        lb*: Value
        ub*: Value
    GenericType* = object
        id*: TypeVarId
        ident*: Ident
        ub*: Value
        typ*: Value
    SymbolKind* {.pure.} = enum
        Var
        Let
        Const
        Param
        Typ
        GenParam
        Func
    SymbolId = int
    Symbol* = ref SymbolObject
    SymbolObject* = object
        id*: Ident
        case kind*: SymbolKind
        of SymbolKind.Var, SymbolKind.Let, SymbolKind.Const, SymbolKind.Param:
            decl_iddef*: IdentDef
            region*: Region
        of SymbolKind.Typ:
            decl_typedef*: TypeDef
        of SymbolKind.GenParam:
            decl_gendef*: GenTypeDef
        of SymbolKind.Func:
            decl_funcdef*: Function
        global*: bool
        val*: Value
        typ*: Value
        use*: seq[Location]
        instances*: Table[Value, Impl]
    Impl* = ref object
        # instance*: Option[Statement]
        lty*: llvm.Type
        val*: llvm.Value

    RegionKind* {.pure.} = enum
        Static # means value type; not ref type
        Global
        Param
        Return
        Suite
        Var
        Link
    RegionObject = object
        case kind*: RegionKind
        of RegionKind.Static:
            nil
        of RegionKind.Global:
            nil
        of RegionKind.Param:
            nth*: Natural
        of RegionKind.Return:
            nil
        of RegionKind.Suite:
            parent*: Region
            # val: LValue
        of RegionKind.Var:
            id*: RegionId
            lb*: Region # indeed, it's true that this is upper bound.
        of RegionKind.Link:
            to*: Region
    RegionId = int

    Region* = ref RegionObject
