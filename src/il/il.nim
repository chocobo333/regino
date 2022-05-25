
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
        For             ## represents for statement
        While           ## while statement
        Loop            ## loop statement
        LetSection      ## declaration of immutable variables
        VarSection      ## declaration of variables
        ConstSection    ## declaration of const values
        TypeSection     ## declaration of types
        Asign           ## assign statement
        Funcdef         ## function definition
        Meta            ## metadata
        Discard         ## discard statement
        Comments        ## comments or docuents
        Expression      ## expression and only this has a type
        Fail            ## occuring compiler-internal error
    Comment* = ref object
        s*: string
        isDoc*: bool
    IdentDefSection* = ref object
        iddefs*: seq[IdentDef]
        comments*: seq[Comment]
    TypeDefSection* = ref object
        typedefs*: seq[TypeDef]
        comments*: seq[Comment]
    Statement* = ref StatementObject
    StatementObject = object
        ## that represents a statement
        loc*: Location
        typ*: Value
        case kind*: StatementKind
        of StatementKind.Loop:
            label*: Option[Ident]
            `block`*: Suite
        of StatementKind.While:
            branch*: ElifBranch
        of StatementKind.LetSection, StatementKind.VarSection, StatementKind.ConstSection:
            iddefSection*: IdentDefSection
        of StatementKind.TypeSection:
            typedefSection*: TypeDefSection
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
            comments*: seq[Comment]
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
        consts*: Table[string, seq[Symbol]] # deprecated
        typeOrder*: Order[Value]  # cumulative
        converters*: Table[(Value, Value), Ident]
    DefKind* {.pure.} = enum
        Def
        Comment
    IdentDef* = ref object
        # represents `pat: typ = default # docStr`
        pat*: Pattern
        typ*: Option[Expression]
        default*: Option[Expression]
        comments*: seq[Comment]
    TypeDef* = ref object
        # represents `pat[params] = typ`
        id*: Ident
        params*: Option[seq[GenTypeDef]]
        typ*: TypeExpression
        comments*: seq[Comment]
    GenTypeDef* = ref object
        # represents `id <: ub`
        id*: Ident
        ub*: Option[Expression]
    FunctionParam* = ref object
        # represents `[implicit](params) -> rety`
        implicit*: seq[GenTypeDef]
        params*: seq[IdentDef]
        rety*: Option[Expression]
        scope*: Scope
    Function* = ref object
        isProp*: bool
        id*: Ident
        param*: FunctionParam
        metadata*: Option[Metadata]
        docStr*: seq[Comment]
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
        IntCast
        Fail
    Expression* = ref ExpressionObject
    ExpressionObject = object
        inserted*: bool
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
            dotArgs*: seq[Expression] # for dot
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
        of ExpressionKind.IntCast:
            int_exp*: Expression
            `from`*: uint
            `to`*: uint
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
        Family
        Sum
        Trait
        Singleton
        Distinct
        Intersection
        Union
        Select
        Lambda
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
            ident*: Ident
            base*: Value
        of ValueKind.ArrayV:
            vals*: seq[Value]
        of ValueKind.Record:
            members*: Table[Ident, Value]
        of ValueKind.Ptr:
            pointee*: Value
        of ValueKind.Pi, ValueKind.Family:
            implicit*: seq[GenericType]
            instances*: seq[Value] # instances of implicit parameters
            params*: seq[Value] # not concerned with Type Family
            rety*: Value
        of ValueKind.Sum:
            constructors*: Table[Ident, Value]
        of ValueKind.Trait:
            paty*: (Pattern, Value)
            iss*: seq[(Pattern, Value)]
            fns*: seq[Function]
        of ValueKind.Intersection, ValueKind.Union, ValueKind.Select:
            id*: TypeVarId
            types*: HashSet[Value]
        of ValueKind.Lambda:
            l_param*: seq[Ident]
            suite*: Suite
        of ValueKind.Cons:
            constructor*: Value
            args*: seq[Value]
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
        Field
        Enum
    SymbolId = int
    Symbol* = ref SymbolObject
    SymbolObject* = object
        id*: Ident                      ## name
        case kind*: SymbolKind          ## kind
        of SymbolKind.Var, SymbolKind.Let, SymbolKind.Const, SymbolKind.Param:
            decl_iddef*: IdentDef       ## declaration
        of SymbolKind.Typ:
            decl_typedef*: TypeDef      ## declaration
        of SymbolKind.GenParam:
            decl_gendef*: GenTypeDef    ## declaration
        of SymbolKind.Func:
            decl_funcdef*: Function                 ## declaration
            constraints*: seq[(Region, Region)]     ## function has some region-constraints concerned its paramteres
        of SymbolKind.Field:
            index*: int
            fielddef*: (Ident, TypeExpression)
        of SymbolKind.Enum:
            enumdef*: SumConstructor
        global*: bool                   ## is global?
        val*: Value                     ## symbol hold a value
        typ*: Value                     ## symbol has a type
        use*: seq[Location]             ## for lsp
        instances*: Table[Value, Impl]  ## Monophasic instances
    Impl* = ref object
        ## llvm implementation of symbol
        instance*: Option[Function] ## Monophasic instance
        lty*: llvm.Type             ## type in llvm
        val*: llvm.Value            ## value in llvm

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
        of RegionKind.Suite:
            parent*: Region
            # val: LValue
        of RegionKind.Param, RegionKind.Return, RegionKind.Var:
            nth*: Natural
            id*: RegionId
            lb*: Region # indeed, it's true that this is upper bound.
        of RegionKind.Link:
            to*: Region
    RegionId = int

    Region* = ref RegionObject
