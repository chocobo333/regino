
import tables
import hashes
import options

import ../../lineinfos

type
    PiType* = object
        ident*: Option[Ident]
        params: seq[GenericType]
        rety: Type

    SymbolKind* {.pure.} = enum
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
        case kind*: SymbolKind
        of SymbolKind.Let, SymbolKind.Var, SymbolKind.Const:
            typ*: Type
        of SymbolKind.Type, SymbolKind.Func, SymbolKind.Field:
            pty*: PiType
            definition*: Function # for Func
            instances*: Table[seq[Type], Symbol]
            index*: int # for Field
    ValueKind* {.pure.} = enum
        Unit
        Univ
        Integer
        Float
        Char
        CString
        Function
    Value* = object
        case kind*: ValueKind
        of ValueKind.Unit:
            nil
        of ValueKind.Univ:
            level*: uint
        of ValueKind.Integer:
            intval: BiggestInt
        of ValueKind.Float:
            floatval: BiggestFloat
            floatbits: uint
        of ValueKind.Char:
            charval*: char
        of ValueKind.CString:
            strval*: string
        of ValueKind.Function:
            fn: Function
    TypeKind* {.pure.} = enum
        Bottom
        Unit
        Univ
        Value
        Integer
        Float
        Char
        CString
        Pair
        Record
        Object
        Arrow
        Cons
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
        of TypeKind.Integer, TypeKind.Float:
            nbits*: uint
        of TypeKind.Char, TypeKind.CString:
            nil
        of TypeKind.Pair:
            first*: Type
            second*: Type
        of TypeKind.Record, TypeKind.Object:
            members*: Table[string, Type]
        of TypeKind.Arrow:
            params*: seq[Type]
            rety*: Type
        of TypeKind.Cons:
            cons*: PiType
            args*: seq[Type]
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
    TypeVar* = object
        id*: VarId
        case kind*: TypeVarKind
        of TypeVarKind.Var:
            ub*: Type
            lb*: Type
        of TypeVarKind.Select:
            choices*: seq[Type]
    GenericType* = object
        ub*: Type
        typ*: Type
    VarId* = int

    Ident* = object
        loc*: Location
        name*: string
    IdentDef* = object
        pat*: Pattern
        typ*: Option[Expression]
        default*: Option[Expression]
    TypeDef* = object
        ident*: Ident
        params*: seq[GenIdentDef]
        typ*: TypeExpression
    GenIdentDef* = object
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
        implicits*: seq[GenIdentDef]
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
        Unit
        Integer
        Float
        Char
        CString
        Ident
        Call
        Apply
        If
        Case
        ObjCons
        Ref

        Import
        LetSection
        VarSection
        TypeSection
        Assign
        IndexAssign
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
    Expression* = ref ExpressionObject
    ExpressionObject = object
        loc*: Location
        typ*: Type
        case kind*: ExpressionKind
        of ExpressionKind.Unit:
            nil
        of ExpressionKind.Integer:
            intval*: BiggestInt
            intbits*: uint
        of ExpressionKind.Float:
            floatval*: BiggestInt
            floatbits*: uint
        of ExpressionKind.Char:
            charval*: char
        of ExpressionKind.CString:
            strval*: string
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
        of ExpressionKind.ObjCons:
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
        of ExpressionKind.IndexAssign:
            iassign_lval*: Expression
            iassign_index*: seq[Expression]
            iassign_val*: Expression
        of ExpressionKind.Funcdef:
            fn*: Function
        of ExpressionKind.ImportLL:
            signature*: FunctionSignature
        of ExpressionKind.Loop, ExpressionKind.Discard:
            lable*: Ident
            `block`*: Expression
        of ExpressionKind.Seq:
            expressions*: seq[Expression]
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
        Ident
        Tuple
        Record
    Pattern* = ref object
        ident*: Ident
        case kind*: PatternKind
        of PatternKind.Ident:
            nil
        of PatternKind.Tuple:
            patterns*: seq[Pattern]
        of PatternKind.Record:
            members*: seq[(Ident, Pattern)]




proc hash*(self: TypeVar): Hash =
    self.id.hash
proc `==`*(self, other: TypeVar): bool =
    self.id == other.id


