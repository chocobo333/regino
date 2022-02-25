
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
        Let
        Var
        Const
        Typedef
        Asign
        Funcdef
        Meta
        Expression
    Statement* = ref StatementObject
    StatementObject = object
        loc*: Location
        case kind*: StatementKind
        of StatementKind.Block, StatementKind.Loop:
            label*: Option[Ident]
            `block`*: Suite
        of StatementKind.While:
            branch*: ElifBranch
        of StatementKind.Let, StatementKind.Var, StatementKind.Const:
            iddef*: IdentDef
        of StatementKind.Typedef:
            iddefs*: seq[IdentDef]
        of StatementKind.For, StatementKind.Asign:
            pat*: Pattern
            val*: Expression
            suite*: Suite # for `for`
        of StatementKind.Funcdef:
            fn*: Function
        of StatementKind.Meta:
            meta*: Metadata
        of StatementKind.Expression:
            expression*: Expression
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
        Pair
        Record
        If
        Case
        Apply
        Dot
        Bracket
        Malloc
    Expression* = ref ExpressionObject
    ExpressionObject = object
        loc*: Location
        typ*: Value
        case kind*: ExpressionKind
        of ExpressionKind.Literal:
            litval*: Literal
        of ExpressionKind.Ident:
            ident*: Ident
        of ExpressionKind.Pair:
            first*: Expression
            second*: Expression
        of ExpressionKind.Record:
            members*: seq[(string, Expression)]
        of ExpressionKind.If:
            elifs*: seq[ElifBranch]
            elseb*: ElseBranch
        of ExpressionKind.Case:
            ofs*: seq[OfBranch]
            default*: ElseBranch
        of ExpressionKind.Apply, ExpressionKind.Bracket:
            callee*: Expression
            args*: seq[Expression]
        of ExpressionKind.Dot:
            lhs*: Expression
            rhs*: Expression
        of ExpressionKind.Malloc:
            mtype*: Expression
            msize*: Expression
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
        Pair
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
        of PatternKind.Pair:
            first*: Pattern
            second*: Pattern
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
            members*: Table[string, Value]
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
        name*: string
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
