
import tables
import sets
import options

import ../lineinfos
import ../orders
from llvm import nil


type
    Program* = seq[Statement]
    StatementKind* {.pure.} = enum
        Block
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
        of StatementKind.Block, StatementKind.Loop:
            label*: Option[Ident]
            `block`*: Suite
        of StatementKind.While:
            branch*: ElifBranch
        of StatementKind.LetSection, StatementKind.VarSection, StatementKind.ConstSection, StatementKind.TypeSection:
            iddefs*: seq[IdentDef]
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
    Suite* = object
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
    IdentDef* = object
        pat*: Pattern
        typ*: Option[Expression]
        default*: Option[Expression]
    FunctionParam* = object
        implicit*: seq[IdentDef]
        params*: seq[IdentDef]
        rety*: Option[Expression]
    Function* = object
        id*: Ident
        param*: FunctionParam
        metadata*: Option[Metadata]
        suite*: Option[Suite]
    ExpressionKind* {.pure.} = enum
        Literal
        Ident
        Tuple
        Seq
        Record
        If
        Case
        Call
        Command
        Dot
        Bracket
        Binary
        Prefix
        Postfix
        Malloc
        Typeof
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
        of ExpressionKind.Tuple, ExpressionKind.Seq:
            exprs*: seq[Expression]
        of ExpressionKind.Record:
            members*: seq[(string, Expression)]
        of ExpressionKind.If:
            elifs*: seq[ElifBranch]
            elseb*: Option[ElseBranch]
        of ExpressionKind.Case:
            ofs*: seq[OfBranch]
            default*: ElseBranch
        of ExpressionKind.Call, ExpressionKind.Command, ExpressionKind.Bracket:
            callee*: Expression
            args*: seq[Expression]
        of ExpressionKind.Dot, ExpressionKind.Binary, ExpressionKind.Prefix, ExpressionKind.Postfix:
            lhs*: Expression
            rhs*: Expression
            op*: Ident # for binary, unary
            expression*: Expression # for unary
        of ExpressionKind.Malloc:
            mtype*: Expression
            msize*: Expression
        of ExpressionKind.Typeof:
            `typeof`*: Expression
        of ExpressionKind.Fail:
            nil
    Typedef* = object
        pat*: Pattern
        typeexpr*: TypeExpression
    TypeExpressionKind* {.pure.} = enum
        Object
        Sum
        Ref
        Trait
        Expression
    TypeExpression* = ref TypeExpressionObject
    TypeExpressionObject = object
        case kind: TypeExpressionKind
        of TypeExpressionKind.Object:
            members*: seq[(Ident, TypeExpression)]
        of TypeExpressionKind.Sum:
            sum*: SumType
        of TypeExpressionKind.Ref:
            pointee*: TypeExpression
        of TypeExpressionKind.Trait:
            trait*: TraitType
        of TypeExpressionKind.Expression:
            expression: Expression
    SumType* = object
        constructors*: SumConstructor
    SumConstructorKind* {.pure.} = enum
        NoFiled
        UnnamedField
        NamedField
    SumConstructor* = ref SumConstructorObject
    SumConstructorObject = object
        id*: Ident
        case kind: SumConstructorKind
        of SumConstructorKind.NoFiled:
            nil
        of SumConstructorKind.UnnamedField:
            types*: seq[TypeExpression]
        of SumConstructorKind.NamedField:
            fields*: seq[(Ident, TypeExpression)]
    TraitType* = object
        traits*: Trait
    Trait* = ref TraitObject
    TraitKind* = enum
        Is
        Func
    TraitObject = object
        case kind: TraitKind
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
    Ident* = object
        name*: string
        loc*: Location
        typ*: Value
    PatternKind* {.pure.} = enum
        Literal
        Ident
        Dot
        Bracket
        Tuple
        Record
        UnderScore
    Pattern* = ref PatternObject
    PatternObject = object
        case kind*: PatternKind
        of PatternKind.Literal:
            litval*: Literal
        of PatternKind.Ident:
            ident*: Ident
        of PatternKind.Dot:
            lhs*: Pattern
            rhs*: Ident
        of PatternKind.Bracket:
            callee*: Pattern
            args*: seq[Expression]
        of PatternKind.Tuple:
            patterns*: seq[Pattern]
        of PatternKind.Record:
            members*: seq[(Ident, Pattern)]
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
    ValueKind {.pure.} = enum
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
        Record
        Ptr
        Pi
        Sum
        Trait
        Singleton
        Distinct
        Intersection
        Union
        Var
        Gen
        Link
    Value* = ref ValueObject
    ValueObject = object
        case kind: ValueKind
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
        of ValueKind.Record:
            members*: Table[Ident, Value]
        of ValueKind.Ptr:
            pointee*: Value
        of ValueKind.Pi:
            implicit*: seq[(Expression, Value)]
            params*: seq[Value]
            rety*: Value
        of ValueKind.Sum:
            cons*: Table[Ident, Value]
        of ValueKind.Trait:
            methods*: Function
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
        id*: Ident
        ub*: Value
        typ*: Value
    SymbolKind* {.pure.} = enum
        Var
        Let
        Const
        Typ
        Func
    SymbolId = int
    Symbol* = ref object
    SymbolObject = object
        id*: SymbolId
        kind*: SymbolKind
        global*: bool
        decl*: Ident
        val*: Value
        typ*: Value
        use*: seq[Location]
        instances*: Table[Value, Impl]
    Impl* = ref object
        # instance*: Option[Statement]
        lty*: llvm.Type
        val*: llvm.Value
