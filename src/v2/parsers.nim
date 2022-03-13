
import sugar
import options
import strutils
import sequtils

import il

import parsers/[
    parsers,
    combinators
]
import lineinfos


type
    TrailerObj = object
        case kind: ExpressionKind
        of ExpressionKind.Call, ExpressionKind.Command, ExpressionKind.Bracket:
            args: seq[Expression]
        of ExpressionKind.Dot:
            field: Expression
        of ExpressionKind.Postfix:
            op: Ident
        else:
            nil

proc Call(_: typedesc[TrailerObj], args: seq[Expression]): TrailerObj =
    TrailerObj(kind: ExpressionKind.Call, args: args)
proc Command(_: typedesc[TrailerObj], args: seq[Expression]): TrailerObj =
    TrailerObj(kind: ExpressionKind.Command, args: args)
proc Bracket(_: typedesc[TrailerObj], args: seq[Expression]): TrailerObj =
    TrailerObj(kind: ExpressionKind.Bracket, args: args)
proc Dot(_: typedesc[TrailerObj], field: Expression): TrailerObj =
    TrailerObj(kind: ExpressionKind.Dot, field: field)
proc Postfix(_: typedesc[TrailerObj], op: Ident): TrailerObj =
    TrailerObj(kind: ExpressionKind.Postfix, op: op)

proc make(self: TrailerObj, exp: Expression, endpos: Position): Expression =
    let loc = newLocation(exp.loc.uri, exp.loc.range.a, endpos)
    case self.kind
    of ExpressionKind.Call:
        Expression.Call(exp, self.args, loc)
    of ExpressionKind.Command:
        Expression.Command(exp, self.args, loc)
    of ExpressionKind.Bracket:
        Expression.Bracket(exp, self.args, loc)
    of ExpressionKind.Dot:
        Expression.Dot(exp, self.field, loc)
    of ExpressionKind.Postfix:
        Expression.Postfix(self.op, exp, loc)
    else:
        Expression.Fail(loc)

proc Fail(self: ref Source): Option[Location] =
    self.pos.map(it => newLocation(self.uri, it, it))
proc Err[T](parser: proc(self: ref Source): Option[T], err: ParseError, loc: Option[Location] = none(Location)): proc(self: ref Source): Option[T] =
    proc parse(self: ref Source): Option[T] =
        let
            (res, loc) = if loc.isNone:
                (%parser)(self)
            else:
                (parser(self), loc.get)
        err.loc = loc
        self.errs.add(loc)
        res
    parse
proc Indent(self: ref Source): Option[int]
proc Dedent(self: ref Source): Option[int]
proc Nodent(self: ref Source): Option[int]
proc Stmt(self: ref Source): Option[Statement]
proc StmtList(self: ref Source): Option[seq[Statement]]
proc Expr(self: ref Source): Option[Expression]
proc TypeExpr(self: ref Source): Option[TypeExpression]
proc Patt(self: ref Source): Option[Pattern]
proc ArgList(self: ref Source): Option[seq[Expression]]

proc makeExpr(it: (Expression, seq[(Ident, Expression)])): Expression =
    result = it[0]
    for (op, right) in it[1]:
        let loc = newLocation(result.loc.uri, result.loc.range.a, right.loc.range.b)
        result = Expression.Binary(op, result, right, loc)
let
    Eof = !p".|\n|\r\n|\r"

    sp0     = sp(0)
    sp1     = sp(1)
    oneline = p".*"
    fun     = s"func"
    ift     = s"if"
    whent   = s"when"
    elift   = s"elif"
    elset   = s"else"
    fort    = s"for"
    intt    = s"in"
    ist     = s"is"
    whilet  = s"while"
    loop    = s"loop"
    falset  = s"false"
    truet   = s"true"
    discar   = s"discard"
    KeyWords = p"let|var|const|if|elif|func|else|for|in|while|when"
    lpar    = s"(" ^ sp(0)
    rpar    = s")" ^ sp(0)
    lcur    = s"{" ^ sp(0)
    rcur    = s"}" ^ sp(0)
    lbra    = s"[" ^ sp(0)
    rbra    = s"]" ^ sp(0)
    colon   = s":" ^ sp(0)
    eq      = s"=" ^ sp(0)
    dot     = s"." ^ sp(0)
    comma   = s"," ^ sp(0)
    us      = s"_" ^ sp(0)
    bikkuri = s"!" ^ sp(0)
    dq      = s("\"")
    strlit  = p("\"(([^\"]|(\\.))*)\"")
    charlit = p("'.'")
    arr     = s"->" ^ sp(0)
    tlt     = s"<:" ^ sp(0)

    arrowop = %p"[\p{Sm}*/\\?!%&$^@-]*(->|=>|~>)" @ (it => newIdent(it[0], it[1]))
    asgnop = %p"[\p{Sm}*/\\?!%&$^@-]*=" @@
        proc(it: Option[(string, Location)]): Option[Ident] =
            if it.isSome:
                let res = it.get
                if res[0][0] in "<>=!":
                    none(Ident)
                else:
                    some newIdent(res[0], res[1])
            else:
                none(Ident)
    symbolop = %p"[@:?][\p{Sm}*/\\?!%&$^@-]*" @@
        proc(it: Option[(string, Location)]): Option[Ident] =
            if it.isSome:
                let res = it.get
                if res[0] == ":":
                    none(Ident)
                else:
                    some newIdent(res[0], res[1])
            else:
                none(Ident)
    orop = %p"(or|xor)|(\|[\p{Sm}*/\\?!%&$^@-]*)" @ (it => newIdent(it[0], it[1]))
    andop = %s"and" @ (it => newIdent(it[0], it[1]))
    cmpop = %p"(not|in|notin|is|isnot|of|as|==)|([<>!][\p{Sm}*/\\?!%&$^@-]*)" @ (it => newIdent(it[0], it[1]))
    rangeop = %s".." @ (it => newIdent(it[0], it[1]))
    ampop = %p"\&[\p{Sm}*/\\?!%&$^@-]*" @ (it => newIdent(it[0], it[1]))
    plusop = %p"[+\-~|][\p{Sm}*/\\?!%&$^@-]*" @@
        proc(it: Option[(string, Location)]): Option[Ident] =
            if it.isSome:
                let
                    res = it.get
                    it = res[0]
                if it.endswith("->") or it.endswith("~>") or it.endswith("=>"):
                    none(Ident)
                else:
                    some newIdent(res[0], res[1])
            else:
                none(Ident)
    mulop = %p"[*/\\%][\p{Sm}*/\\?!%&$^@-]*" @ (it => newIdent(it[0], it[1]))
    powerop = %p"[$^][\p{Sm}*/\\?!%&$^@-]*" @ (it => newIdent(it[0], it[1]))

    Id0 = alt(
        p"[_\p{L}\p{Nl}ー][_\p{L}\p{N}ー]*",
        p"[_\p{L}\p{Sm}\p{Nl}ー\p{Sm}*/\\?!%&$^@-][_\p{L}\p{Sm}\p{N}ー\p{Sm}*/\\?!%&$^@-]*" ^ s"`"
    )
    Int0 = p"0|[1-9][0-9]*"
    IntSuffix = preceded(s"'i", Int0)
    Operators0 = p"[\p{Sm}*/\\?!%&$^@-]+"

    Id = %Id0 @ (it => newIdent(it[0], it[1]))
    Operators = %Operators0 @ (it => newIdent(it[0], it[1]))

    NewLine = preceded(+(sp0 > p"\n"), sp0) @ (it => it.len)

    Comment = preceded(s"#", p".*")
    Comments = %(Comment ^+ Nodent) @ (it => Statement.Comments(it[0], it[1]))

    asop = %p"[\p{Sm}*/\\?!%&$^@-]*=" @ (it => newIdent(it[0], it[1]))
    Asign = %(Patt + asop ^ sp0 + Expr) @
        proc(it: (((Pattern, Ident), Expression), Location)): Statement =
            let
                (it, loc) = it
                (patop, exp) = it
                (pat, op) = patop
            Statement.Asign(pat, op, exp, loc)

    Int = alt(
        Int0 + IntSuffix    @ (it => Literal.integer(it[0].parseInt, it[1].parseInt.uint)),
        Int0                @ (it => Literal.integer(it.parseInt))
    )
    Float = alt(
        Int0 > dot > Int0   @ (it => Literal.floating(parseFloat(it[0] & "." & it[2]))),
        Int0 > dot > !Id0   @ (it => Literal.floating(parseFloat(it[0]))),
        dot > Int0          @ (it => Literal.floating(parseFloat("." & it[1])))
    )
    Boolean = alt(
        falset  @ (it => Literal.boolean(false)),
        truet   @ (it => Literal.boolean(true))
    )
    String = strlit     @ (it => Literal.str(it[1..^2]))
    Char = charlit      @ (it => Literal.char(it[1]))
    Literal = alt(
        Float,
        Int,
        Boolean,
        String,
        Char,
    )

    Tuple = %delimited(lpar, Expr^*(comma), ?comma+rpar) @ (it => Expression.Tuple(it[0], it[1]))
    Record = %delimited(
        lpar,
        (Id + ?(preceded(colon, Expr)))^*comma,
        ?comma+rpar
    ) @
        proc(it: (seq[(Ident, Option[Expression])], Location)): Expression =
            let
                members = it[0].mapIt(
                    (
                        it[0],
                        if it[1].isSome:
                            it[1].get
                        else:
                            Expression(kind: ExpressionKind.Ident, ident: it[0], loc: it[0].loc)
                    )
                )
            Expression.Record(members, it[1])

    Typeof = %delimited(s"typeof" + lpar, Expr, rpar) @ (it => Expression.Typeof(it[0], it[1]))
    Malloc = %delimited(s"malloc" + lpar, Expr + preceded(comma, Expr), rpar) @ (it => Expression.Malloc(it[0][0], it[0][1], it[1]))
    Ref = %preceded(s"ref" + sp1, Expr) @ (it => Expression.Ref(it[0], it[1]))
    FnType = %(delimited(s"func" + lpar, Expr ^* comma, rpar) + preceded(arr, Expr)) @ (it => Expression.FnType(it[0][0], it[0][1], it[1]))

    Atom = %(?Operators + alt(
        %Literal @ (it => Expression.literal(it[0], it[1])),
        Typeof, Malloc, Ref,
        FnType,
        Id @ (it => Expression(kind: ExpressionKind.Ident, ident: it, loc: it.loc)),
        delimited(lpar, Expr, rpar),
        Tuple,
        Record
    )) @
        proc(it: ((Option[Ident], Expression), Location)): Expression =
            let
                (it, loc) = it
                (op, atom) = it
            if op.isSome:
                Expression.Prefix(op.get, atom, loc)
            else:
                atom

    Discard = %alt(
        preceded(
            discar + sp1,
            Expr
        ) @ (it => some it),
        discar @ (it => none(Expression)),
    ) @ (it => Statement.Discard(it[0], it[1]))

    TrailerNotCommand = alt(
        delimited(!sp1, Operators, !(Atom ^ sp0))   @ (it => TrailerObj.Postfix(it)),
        preceded(dot, Atom)                         @ (it => TrailerObj.Dot(it)),
        delimited(lpar, ArgList, rpar)              @ (it => TrailerObj.Call(it)),
        delimited(lbra, ArgList, rbra)              @ (it => TrailerObj.Bracket(it)),
    )
    AtomExprNotCommand = Atom + *(TrailerNotCommand + pos) @
        proc(it: (Expression, seq[(TrailerObj, Position)])): Expression =
            result = it[0]
            for (trailer, pos) in it[1]:
                result = trailer.make(result, pos)
    ArgList1 = alt(
        %(AtomExprNotCommand + preceded(sp1 + !Operators, Expr)) @
            proc(it: ((Expression, Expression), Location)): Expression =
                Expression.Command(it[0][0], @[it[0][1]], it[1]),
        @Expr,
    ) ^+ comma
    Trailer = alt(
        delimited(!sp1, Operators, !(Atom ^ sp0)) @ (it => TrailerObj.Postfix(it)),
        preceded(dot, %Int @ (it => Expression.literal(it[0], it[1]))) @
            (it => TrailerObj.Dot(it)),
        preceded(dot, Atom) @ (it => TrailerObj.Dot(it)),
        preceded(sp1 + !terminated(Operators, sp1), ArgList1) @ (it => TrailerObj.Command(it)),
        delimited(lpar, ArgList, rpar)                  @ (it => TrailerObj.Call(it)),
        delimited(lbra, ArgList, rbra)                  @ (it => TrailerObj.Bracket(it)),
    )
    AtomExpr = Atom + *(Trailer + pos) @
        proc(it: (Expression, seq[(TrailerObj, Position)])): Expression =
            result = it[0]
            for (trailer, pos) in it[1]:
                result = trailer.make(result, pos)

    PowerExpr = AtomExpr + *(powerop ^ sp0 + AtomExpr)  @ makeExpr
    MulExpr = PowerExpr + *(mulop ^ sp0 + PowerExpr)    @ makeExpr
    PlusExpr = MulExpr + *(plusop ^ sp0 + MulExpr)      @ makeExpr
    AmpExpr = PlusExpr + *(ampop ^ sp0 + PlusExpr)      @ makeExpr
    RangeExpr = AmpExpr + *(rangeop ^ sp0 + AmpExpr)    @ makeExpr
    CmpExpr = RangeExpr + *(cmpop ^ sp0 + RangeExpr)    @ makeExpr
    AndExpr = CmpExpr + *(andop ^ sp0 + CmpExpr)        @ makeExpr
    OrExpr = AndExpr + *(orop ^ sp0 + AndExpr)          @ makeExpr
    SymbolExpr = OrExpr + *(symbolop ^ sp0 + OrExpr)    @ makeExpr
    AsgnExpr = SymbolExpr + *(asgnop ^ sp0 + SymbolExpr) @ makeExpr
    ArrowExpr = AsgnExpr + *(arrowop ^ sp0 + AsgnExpr)  @ makeExpr
    SimplExpr = alt(
        ArrowExpr,
        # ArrowExpr > Suite                               @ (it => akCall.newTreeNode(it))
    )
    IdPattern = Id + ?delimited(lbra, Expr,rbra) @ (it => Pattern(kind: PatternKind.Ident, ident: it[0], index: it[1]))
    UnderScore = s"_" @ (it => Pattern.UnderScore)
    LiteralPattern = Literal @ (it => Pattern.literal(it))
    TuplePattern = (?Id + delimited(lpar, Patt ^* comma, ?comma+rpar)) @ (it => Pattern(kind: PatternKind.Tuple, tag: it[0], patterns: it[1]))
    RecordPattern = (?Id + delimited(
        lpar,
        (Id + ?(preceded(colon, Patt)) @
            proc(it: (Ident, Option[Pattern])): (Ident, Pattern) =
                if it[1].isSome:
                    (it[0], it[1].get)
                else:
                    (it[0], Pattern(kind: PatternKind.Ident, ident: it[0]))
        ) ^* comma,
        ?comma+rpar
    )) @ (it => Pattern(kind: PatternKind.Record, tag: it[0], members: it[1]))
    # BracketPattern: AstNode = IdPattern > delimited(lbra, separated1(Pattern, comma), rbra)    @ (it => akBracketExpr.newTreeNode(it))


    IdentDef = alt(
        Patt + alt(
            ?preceded(colon, Expr) + (preceded(eq, Expr) @ (it => some it)),
            (preceded(colon, Expr) @ (it => some it)) + ?preceded(eq, Expr)
        ) @ (it => newIdentDef(it[0], it[1][0], it[1][1])),
        Patt @ (it => newIdentDef(it))
    )
    GenIdentDef = alt(
        Patt + alt(
            ?preceded(colon, Expr) + (preceded(tlt, Expr) @ (it => some it)),
            (preceded(colon, Expr) @ (it => some it) ) + ?preceded(eq, Expr)
        ) @ (it => newIdentDef(it[0], it[1][0], it[1][1])),
        Patt @ (it => newIdentDef(it))
    )
    GenParamList = GenIdentDef ^* comma
    ParamList = IdentDef ^* comma
    GenParams = delimited(
        lbra,
        GenParamList,
        rbra
    )
    Params = delimited(
        lpar,
        ParamList,
        rpar
    ) + ?(preceded(arr, Expr))
    Suite = alt(
        preceded(
            colon,
            alt(
                Stmt @ (it => @[it]),
                delimited(Indent, StmtList, Dedent),
                # terminated(Fail, delimited(Indent, oneline ^* Nodent, Dedent)) @ (it => (errs.add ParseError(loc: it.loc, msg: @["expression"]); it)),
                # terminated(Fail, alt(&Nodent, Eof @ (it => 0), Dedent)) @ (it => (errs.add ParseError(loc: it.loc, msg: @["expression"]); it))
            )
        ),
        # preceded(
        #     sp0,
        #     Fail + alt(
        #         Statement @ (it => akStmtList.newTreeNode(@[it])),
        #         delimited(Indent, StmtList, Dedent),
        #         terminated(Fail, delimited(Indent, oneline ^* Nodent, Dedent)),
        #         terminated(Fail, alt(&Nodent, Eof @ (it => 0), Dedent))
        #     ) @ (it => (errs.add ParseError(loc: it[0].loc, msg: @["\":\""]);it[1]))
        # ),
    ) @ (it => newSuite(it))
    NoBody = sp0 + &NewLine @ (it => none(il.Suite))
    Metadata = preceded(
        bikkuri,
        delimited(
            lbra,
            Id + ?preceded(colon, Expr),
            rbra
        )
    ) @
        proc(it: (Ident, Option[Expression])): Metadata =
            let
                (id, exp) = it
                args = if exp.isSome: @[exp.get] else: @[]
            case id.name
            of "link":
                Metadata.Link(args)
            of "importll":
                Metadata.ImportLL(args)
            of "subtype":
                Metadata.SubType(args)
            else:
                Metadata.UserDef(id.name, args)
    FuncDef = (terminated(alt(fun @ (it => false), s"prop" @ (it => true)), sp1) +
        Id + ?GenParams + Params + ?Metadata + alt(NoBody, Suite @ (it => some it))
    ) @ (it => (it[0][0][0][0][0], it[0][0][0][0][1], it[0][0][0][1], it[0][0][1], it[0][1], it[1])) @
        proc(it: (bool, Ident, Option[seq[IdentDef]], (seq[IdentDef], Option[Expression]), Option[il.Metadata], Option[il.Suite])): Function =
            let
                (isProp, id, imp, prms, meta, body) = it
            Function(
                isProp: isProp,
                id: id,
                param: FunctionParam(implicit: imp.get(@[]), params: prms[0], rety: prms[1]),
                metadata: meta,
                suite: body
            )

    Section = alt(
        preceded(sp1, IdentDef) @ (it => @[it]),
        delimited(Indent, IdentDef ^+ Nodent, Dedent)
    )
    ObjectElement = Id + preceded(colon, TypeExpr)
    ObjectType = delimited(s"object" + Indent, ObjectElement ^* Nodent, Dedent) @ (it => TypeExpression.Object(it))
    DistinctType = preceded(s"distinct" + sp1, TypeExpr) @ (it => TypeExpression.Distinct(it))
    SumElement = alt(
        Id + Tuple @ (it => SumType.UnnamedField(it[0], it[1].exprs)),
        Id + Record @ (it => SumType.NamedField(it[0], it[1].members)),
        Id @ (it => SumType.NoField(it)),
    )
    SumType = delimited(s"variant" + Indent, SumElement ^+ Nodent, Dedent) @ (it => TypeExpression.Sum(newSumType(it)))
    IsTrait = Patt + preceded(ist ^ sp0, TypeExpr) @ (it => Trait.Is(it[0], it[1]))
    FnTrait = FuncDef @ (it => Trait.Func(it))
    TraitElement = alt(
        IsTrait,
        FnTrait,
    )
    TraitType = preceded(s"trait" + sp1, Patt + preceded(colon, Expr)) + ?delimited(s"with" ^ sp0 + Indent, TraitElement ^+ Nodent, Dedent) @ (it => TypeExpression.TraitT(newTrait(it[0][0], it[0][1], it[1].get(@[]))))

    TypeDef = Id + ?GenParams + preceded(eq, TypeExpr) @ (it => newTypeDef(it[0][0], it[0][1], it[1]))

    LetSection = %preceded(s"let", Section) @ (it => Statement.LetSection(it[0], it[1]))
    VarSection = %preceded(s"var", Section) @ (it => Statement.Varsection(it[0], it[1]))
    ConstSection = %preceded(s"const", Section) @ (it => Statement.ConstSection(it[0], it[1]))
    TypeSection = %preceded(
        s"type",
        alt(
            preceded(sp1, TypeDef) @ (it => @[it]),
            delimited(Indent, TypeDef ^+ Nodent, Dedent)
        )
    ) @ (it => Statement.TypeSection(it[0], it[1]))

#     # epression
    CondBranch = alt(
        terminated(Expr, sp0) + Suite,
        # preceded(sp0, Fail > Suite) @ (it => (errs.add ParseError(loc: it[0].loc, msg: @["expression"]); it))
    ) @ (it => newElif(it[0], it[1]))
    ElifBranch = preceded(elift > sp1, CondBranch)
    ElseBranch = preceded(terminated(elset, sp0), Suite)

    IfExpr = %(preceded(ift > sp1, CondBranch) + *preceded(Nodent, ElifBranch) + ?preceded(Nodent, ElseBranch)) @
        proc(it: (((ElifBranch, seq[ElifBranch]), Option[il.ElseBranch]), Location)): Expression =
            let
                (branches, loc) = it
                (elifs, elseb) = branches
                (ifb, elifb) = elifs
            Expression.If(@[ifb] & elifb, elseb, loc)
    WhenExpr = %(preceded(whent > sp1, CondBranch) + *preceded(Nodent, ElifBranch) + ?preceded(Nodent, ElseBranch)) @
        proc(it: (((ElifBranch, seq[ElifBranch]), Option[il.ElseBranch]), Location)): Expression =
            let
                (branches, loc) = it
                (elifs, elseb) = branches
                (ifb, elifb) = elifs
            Expression.When(@[ifb] & elifb, elseb, loc)
    OfBranch = preceded(s"of" + sp1, Patt) + Suite @ (it => newOf(it[0], it[1]))
    CaseExpr = %(delimited(s"case" + sp1, Expr, ?colon) + ?preceded(Nodent, OfBranch ^+ Nodent) + ?preceded(Nodent + s"default", Suite)) @
        proc(it: (((Expression, Option[seq[il.OfBranch]]), Option[il.Suite]), Location)): Expression =
            Expression.Case(it[0][0][0], it[0][0][1].get(@[]), it[0][1], it[1])

    LoopStmt = preceded(
        loop > colon,
        alt(
            delimited(
                Indent,
                StmtList,
                Dedent
            ),
            Stmt @ (it => @[it])
        ) @ (it => newSuite(it))
    ) @ (it => Statement.Loop(it))
    LambdaDef = %(preceded(s"func", Params) + Suite) @ (it => Expression.Lambda(it[0][0], it[0][1], it[1]))


#     # atom


#     Id = Id0    @ (it => (if it.fragment == "_": akUs.newNode() else: newIdNode(it.fragment, it.toLocation(uri))))

proc Indent(self: ref Source): Option[int] =
    let it = self.NewLine
    if it.isSome:
        if self.indent[^1] < it.get:
            self.indent.add it.get
            it
        else:
            none(int)
    else:
        it
proc Dedent(self: ref Source): Option[int] =
    let it = (&NewLine)(self)
    if it.isSome:
        let n = it.get
        if n in self.indent and n != self.indent[^1]:
            discard self.indent.pop
            it
        else:
            none(int)
    else:
        it
proc Nodent(self: ref Source): Option[int] =
    let it = self.NewLine
    if it.isSome:
        if self.indent[^1] == it.get:
            it
        else:
            none(int)
    else:
        it

proc Program*(self: ref Source): Option[Program] =
    alt(
        delimited(
            ?Nodent,
            StmtList,
            preceded(?Nodent, Eof)
        ),
        %success(seq[Statement]) @
            proc(it: (seq[Statement], Location)): seq[Statement] =
                self.errs.add ParseError(loc: it[1], msg: @["program"])
                it[0]
    )(self)

proc StmtList(self: ref Source): Option[seq[Statement]] =
    let
        nl = alt(
            @Nodent,
            preceded(!Dedent, %NewLine) @
                proc(it: (int, Location)): int =
                    self.errs.add ParseError(kind: InvalidIndentation, loc: it[1])
                    it[0]
        )
        parser = Stmt ^+ nl
    parser(self)
proc Stmt(self: ref Source): Option[Statement] =
    alt(
        Comments,
        LetSection,
        VarSection,
        ConstSection,
        TypeSection,
        LoopStmt,
        %FuncDef @ (it => il.Funcdef(Statement, it[0], it[1])),
        Discard,
        %Metadata @ (it => Statement.Meta(it[0], it[1])),
        Asign,
        Expr @ (it => it.Statement),
        # terminated(success(Statement), oneline) @
        #     proc(it: Statement): Statement =
        #         self.errs.add ParseError(loc: it.loc, msg: @["statement"])
        #         it
    )(self)

proc Expr(self: ref Source): Option[Expression] =
    alt(
        IfExpr,
        WhenExpr,
        CaseExpr,
        LambdaDef,
        # ForExpr,
        SimplExpr
    )(self)
proc TypeExpr(self: ref Source): Option[TypeExpression] =
    let
        isRef = (s"ref" + sp1)(self)
        typ = alt(
            ObjectType,
            SumType,
            TraitType,
            Expr @ (it => TypeExpression.Expr(it))
        )(self)
    proc r(self: TypeExpression): TypeExpression =
        result = self
        result.isRef = true
    if isRef.isSome:
        typ.map(r)
    else:
        typ

proc Patt(self: ref Source): Option[Pattern] =
    alt(
        LiteralPattern,
        TuplePattern,
        RecordPattern,
        UnderScore,
        IdPattern,
    )(self)
proc ArgList(self: ref Source): Option[seq[Expression]] =
    let parser = alt(
        %(AtomExprNotCommand + preceded(sp1 + !Operators, Expr)) @
            proc(it: ((Expression, Expression), Location)): Expression =
                Expression.Command(it[0][0], @[it[0][1]], it[1]),
        @Expr,
    ) ^* comma
    parser(self)

export parsers

when isMainModule:
    let
        src = Source.from(
            """
![link: "arith.ll"]
![link: "io.ll"]
![link: "int.ll"]
![link: "string.ll"]

type
    bool = typeof(false)
    int = typeof(0)
    int32 = typeof(0'i32)
    float = typeof(0.0)
    string = typeof("")
    char = typeof('a')

    ptr[T] = typeof(malloc(T, 1))

type
    A = ref bool
    yen = distinct int
    B = object
        a: int
        b: float
    C = variant
        A
        B(a, b)
    List[T] = ref variant
        Nil
        Cons(T, List[T])
    Tree[T] = ref variant
        Leaf(T)
        Node(lhs: Tree[T], rhs: Tree[T])

type
    pair[T, U] = (T, U)


if true:
    3
    if a:
        10
elif false:
    5
else:
    4
let a = 1


var
    b = 3
    c = 4 * 3
a = 3
discard a
loop:
    echo 3
func `$`(a: bool) -> string ![subtype]:
    if a:
        "true"
    else:
        "false"
a[3] = 3
let
    a: func(bool) -> string = `$`
    a: b = 3
    a: ref typeof(3)
type
    Magma = trait (T, op): (Type, func(T, T) -> T)
    SemiGroup = trait (T, op): (Type, func(T, T) -> T) with
        prop associtive(x, y, z: T) -> op(x, op(y, z)) == op(op(x, y), z)
when true:
    3
func(a: bool): 3
func sum(l: List[int32]) -> int32:
    case l
    of Nil:
        0
    of Cons(head, tail):
        head + sum(tail)
"""
        )
        p = Program(src).get
    echo p
    echo src.errs
