
import tables
import sets
import options

import ../../lineinfos

type
    PiType* = object
        ident*: Option[Ident]
        params*: seq[GenericType]
        rety*: Type

    SymbolKind* {.pure.} = enum
        Notdeclared
        Let
        Var
        Const
        Type
        Func
        Field
    Symbol* = object
        loc*: Location
        ident*: Ident
        val*: Type
        global*: bool
        case kind*: SymbolKind
        of SymbolKind.Notdeclared, SymbolKind.Let, SymbolKind.Var, SymbolKind.Const:
            typ*: Type
        of SymbolKind.Type, SymbolKind.Func, SymbolKind.Field:
            pty*: PiType
            definition*: Function # for Func
            instances*: Table[seq[Type], Symbol]
            index*: int # for Field
    ValueKind* {.pure.} = enum
        Unit
        Univ
        Bool
        Integer
        Float
        Char
        CString
        Array
        Function
    Value* = object
        case kind*: ValueKind
        of ValueKind.Unit:
            nil
        of ValueKind.Univ:
            level*: uint
        of ValueKind.Bool:
            boolval*: bool
        of ValueKind.Integer:
            intval*: BiggestInt
            intbits*: uint
        of ValueKind.Float:
            floatval*: BiggestFloat
            floatbits*: uint
        of ValueKind.Char:
            charval*: char
        of ValueKind.CString:
            strval*: string
        of ValueKind.Array:
            vals*: seq[Type]
        of ValueKind.Function:
            fn*: Function
    TypeKind* {.pure.} = enum
        Bottom
        Unit
        Univ
        Value
        Bool
        Integer
        Float
        Char
        CString
        Pair
        Array
        Record
        Object
        Arrow
        Cons
        Distinct
        Singleton
        Ptr
        Recursive
        Trait
        Var
        Gen
        Link
    Type* = ref TypeObject
    TypeObject = object
        symbol*: Option[Symbol]
        case kind*: TypeKind
        of TypeKind.Bottom, TypeKind.Unit:
            nil
        of TypeKind.Univ:
            level*: uint
        of TypeKind.Value:
            val*: Value
        of TypeKind.Bool:
            nil
        of TypeKind.Integer, TypeKind.Float:
            nbits*: uint
        of TypeKind.Char, TypeKind.CString:
            nil
        of TypeKind.Pair:
            first*: Type
            second*: Type
        of TypeKind.Record, TypeKind.Object:
            members*: Table[string, Type]
        of TypeKind.Array, TypeKind.Distinct, Singleton:
            length*: uint # for Array
            base*: Type
        of TypeKind.Ptr:
            pointee*: Type
        of TypeKind.Arrow:
            params*: seq[Type]
            rety*: Type
        of TypeKind.Cons:
            cons*: PiType
            args*: seq[Type]
        of TypeKind.Recursive:
            self*: VarId
            body*: Type
        of TypeKind.Trait:
            paty*: (Pattern, Type)
            iss*: seq[(Pattern, Value)]
            fns*: seq[Function]
            fnss*: seq[FunctionSignature]
        of TypeKind.Var:
            tv*: TypeVar
        of TypeKind.Gen:
            gt*: GenericType
        of TypeKind.Link:
            to*: Type
    TypeVarKind* {.pure.} = enum
        Var
        Select
        Recursive
    TypeVar* = object
        id*: VarId
        case kind*: TypeVarKind
        of TypeVarKind.Var:
            ub*: Type
            lb*: Type
        of TypeVarKind.Select:
            choices*: HashSet[Type]
        of TypeVarKind.Recursive:
            nil
    GenericType* = object
        id*: VarId
        ub*: Type
        typ*: Type
    VarId* = int

    Scope* = ref object
        parent*: Scope
        vars*: Table[string, Symbol]
        types*: Table[string, Symbol]
        funcs*: Table[string, seq[Symbol]]
    Ident* = ref object
        loc*: Location
        name*: string
        typ*: Type
    IdentDef* = object
        pat*: Pattern
        typ*: Option[Expression]
        default*: Option[Expression]
    TypeDef* = object
        ident*: Ident
        params*: seq[GenTypeDef]
        typ*: TypeExpression
    GenTypeDef* = object
        ident*: Ident
        typ*: Option[Expression]
        ub*: Option[Expression]
    MetadataKind* {.pure.} = enum
        SubType
    Metadata* = object
        case kind*: MetadataKind
        of MetadataKind.SubType:
            nil
    FunctionSignature* = object
        ident*: Ident
        implicits*: seq[GenTypeDef]
        params*: seq[IdentDef]
        rety*: Expression
    Function* = object
        signature*: FunctionSignature
        body*: Expression
        metadata*: seq[Metadata]
    TypeExpressionKind* {.pure.} = enum
        Ref
        Object
        Variant
        Trait
        Expression
    TypeExpression* = ref object
        case kind*: TypeExpressionKind
        of TypeExpressionKind.Ref:
            to*: TypeExpression
        of TypeExpressionKind.Object:
            ident*: Ident
            members*: Table[Ident, Expression]
        of TypeExpressionKind.Variant:
            elements*: seq[VariantElement]
        of TypeExpressionKind.Trait:
            paty*: (Pattern, Expression)
            iss*: seq[(Pattern, Expression)]
            fns*: seq[Function]
            fnss*: seq[FunctionSignature]
        of TypeExpressionKind.Expression:
            expression*: Expression
    VariantElementKind* {.pure.} = enum
        NoField
        Tuple
        Object
    VariantElement* = object
        ident*: Ident
        case kind*: VariantElementKind
        of VariantElementKind.NoField:
            nil
        of VariantElementKind.Tuple:
            fields*: seq[Expression]
        of VariantElementKind.Object:
            members*: Table[Ident, Expression]

    ExpressionKind* {.pure.} = enum
        Literal
        Ident
        Call
        Apply
        If
        Case
        Pair
        Array
        Record
        ObjCons
        Ref

        Import
        LetSection
        VarSection
        TypeSection
        Assign
        Funcdef
        ImportLL
        Loop
        Discard
        Seq

        Typeof
        Malloc
        Realloc
        PtrSet
        PtrGet
    PremitiveExpressionKind = range[ExpressionKind.Typeof..ExpressionKind.PtrGet]
    LiteralKind* {.pure.} = enum
        Unit
        Bool
        Integer
        Float
        Char
        CString
    Literal* = object
        case kind*: LiteralKind
        of LiteralKind.Unit:
            nil
        of LiteralKind.Bool:
            boolval*: bool
        of LiteralKind.Integer:
            intval*: BiggestInt
            intbits*: uint
        of LiteralKind.Float:
            floatval*: BiggestFloat
            floatbits*: uint
        of LiteralKind.Char:
            charval*: char
        of LiteralKind.CString:
            strval*: string
    Expression* = ref ExpressionObject
    ExpressionObject = object
        loc*: Location
        typ*: Type
        case kind*: ExpressionKind
        of ExpressionKind.Literal:
            litval*: Literal
        of ExpressionKind.Ident:
            ident*: Ident
        of ExpressionKind.Call, ExpressionKind.Apply:
            callee*: Expression
            args*: seq[Expression]
        of ExpressionKind.If:
            cond*: Expression
            then*: Expression
            els*: Expression
        of ExpressionKind.Case:
            ofs*: (Pattern, Expression)
        of ExpressionKind.Pair:
            first*: Expression
            second*: Expression
        of ExpressionKind.Array:
            elements*: seq[Expression]
        of ExpressionKind.Record, ExpressionKind.ObjCons:
            obj*: Expression
            implicits*: seq[Expression]
            members*: Table[Ident, Expression]
        of ExpressionKind.Ref:
            to*: Expression
        of ExpressionKind.Import:
            module*: Ident
        of ExpressionKind.LetSection, ExpressionKind.VarSection:
            iddefs*: seq[IdentDef]
        of ExpressionKind.TypeSection:
            typedefs*: seq[TypeDef]
        of ExpressionKind.Assign:
            assign_lval*: Pattern
            assign_val*: Expression
        of ExpressionKind.Funcdef:
            fn*: Function
        of ExpressionKind.ImportLL:
            signature*: FunctionSignature
        of ExpressionKind.Loop, ExpressionKind.Discard:
            lable*: Ident
            `block`*: Expression
        of ExpressionKind.Seq:
            expressions*: seq[Expression]
            scope*: Scope
        of ExpressionKind.Typeof:
            `typeof`*: Expression
        of ExpressionKind.Malloc, ExpressionKind.Realloc:
            mtype*: Expression # for Malloc
            rptr*: Expression # for Realloc
            msize*: Expression
        of ExpressionKind.PtrSet, ExpressionKind.PtrGet:
            `ptr`*: Expression
            index*: Expression
            val*: Expression # for PtrSet


    PatternKind* {.pure.} = enum
        Literal
        Ident
        Tuple
        Record
    Pattern* = ref object
        case kind*: PatternKind
        of PatternKind.Literal:
            litval*: Literal
        of PatternKind.Ident:
            ident*: Ident
        of PatternKind.Tuple, PatternKind.Record:
            tag*: Option[Ident]
            patterns*: seq[Pattern]         # for Tuple
            members*: seq[(Ident, Pattern)] # for Record




proc `==`*(self, other: TypeVar): bool =
    self.id == other.id


