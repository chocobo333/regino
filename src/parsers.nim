
import strutils
import strformat
import sequtils
import options
import sugar

import eat except Parser
import eat/dsl

import ast
import lineinfos
import uri



proc first[T](a: seq[T]): T = a[0]
proc second[T](a: seq[T]): T = a[1]
proc boxing[T](a: T): seq[T] = @[a]
proc newTreeNode(kind: range[akFailed..akPatterns]): proc(a: seq[AstNode]): AstNode =
    result = proc(a: seq[AstNode]): AstNode =
        kind.newTreeNode(a)

type
    ParseErrorKind = enum
        Expected
        InvalidIndentation
    ParseError* = object
        loc*: Location
        msg: seq[string]
        kind: ParseErrorKind

proc empty(self: typedesc[int]): int = -1

proc `$`*(self: ParseError): string =
    result = fmt"{self.loc} "
    result.add case self.kind
    of Expected:
        fmt"expected: {self.msg[0]}"
    of InvalidIndentation:
        "invalid indentation"
ParserDef Parser(uri: Uri, indent: seq[int], errs: seq[ParseError]):
    uri = initUri()
    indent = @[0]

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

    arrowop = p"[\p{Sm}*/\\?!%&$^@-]*"              @@ proc(it: PResult[Spanned]): PResult[AstNode] =
                                                        if it.isErr:
                                                            err(it.getSrc, it.getErr)
                                                        else:
                                                            if it.get.fragment.endswith("->") or it.get.fragment.endswith("~>") or it.get.fragment.endswith("=>"):
                                                                ok(it.getSrc, newIdNode(it.get.fragment).seta(it.get.pos).setb(it.get.endpos))
                                                            else:
                                                                err(it.getSrc, "")
    asgnop = p"[\p{Sm}*/\\?!%&$^@-]*="              @@ proc(it: PResult[Spanned]): PResult[AstNode] =
                                                        if it.isErr:
                                                            err(it.getSrc, it.getErr)
                                                        else:
                                                            if it.get.fragment[0] in "<>=!":
                                                                err(it.getSrc, "")
                                                            else:
                                                                ok(it.getSrc, newIdNode(it.get.fragment).seta(it.get.pos).setb(it.get.endpos))
    symbolop = p"[@:?][\p{Sm}*/\\?!%&$^@-]*"        @@ proc(it: PResult[Spanned]): PResult[AstNode] =
                                                        if it.isErr:
                                                            err(it.getSrc, it.getErr)
                                                        else:
                                                            if it.get.fragment == ":":
                                                                err(it.getSrc, "`:` is not operator")
                                                            else:
                                                                ok(it.getSrc, newIdNode(it.get.fragment).seta(it.get.pos).setb(it.get.endpos))
    orop = p"(or|xor)|(\|[\p{Sm}*/\\?!%&$^@-]*)"    @ (it => newIdNode(it.fragment).seta(it.pos).setb(it.endpos))
    andop = s"and"                                  @ (it => newIdNode(it.fragment).seta(it.pos).setb(it.endpos))
    cmpop = p"(not|in|notin|is|isnot|of|as|==)|([<>!][\p{Sm}*/\\?!%&$^@-]*)" @ (it => newIdNode(it.fragment).seta(it.pos).setb(it.endpos))
    rangeop = s".."                                 @ (it => newIdNode(it.fragment).seta(it.pos).setb(it.endpos))
    ampop = p"\&[\p{Sm}*/\\?!%&$^@-]*"              @ (it => newIdNode(it.fragment).seta(it.pos).setb(it.endpos))
    plusop = p"[+\-~|][\p{Sm}*/\\?!%&$^@-]*"        @@ proc(it: PResult[Spanned]): PResult[AstNode] =
                                                        if it.isErr:
                                                            err(it.getSrc, it.getErr)
                                                        else:
                                                            if it.get.fragment.endswith("->") or it.get.fragment.endswith("~>") or it.get.fragment.endswith("=>"):
                                                                err(it.getSrc, "")
                                                            else:
                                                                ok(it.getSrc, newIdNode(it.get.fragment).seta(it.get.pos).setb(it.get.endpos))
    mulop = p"[*/\\%][\p{Sm}*/\\?!%&$^@-]*"         @ (it => newIdNode(it.fragment).seta(it.pos).setb(it.endpos))
    powerop = p"[$^][\p{Sm}*/\\?!%&$^@-]*"          @ (it => newIdNode(it.fragment).seta(it.pos).setb(it.endpos))

    Id0 = alt(
        p"[_\p{L}\p{Nl}ー][_\p{L}\p{N}ー]*",
        s"`" > p"[_\p{L}\p{Sm}\p{Nl}ー\p{Sm}*/\\?!%&$^@-][_\p{L}\p{Sm}\p{N}ー\p{Sm}*/\\?!%&$^@-]*" > s"`" @ second
    )                                                   #@@ proc(it: PResult[Spanned]): PResult[Spanned] =
                                                        #    if it.isErr:
                                                        #        it
                                                        #    else:
                                                        #        if it.get.fragment in ["func", "if", "elif", "let", "var", "const"]:
                                                        #            err(it.getSrc, "keyword")
                                                        #        else:
                                                        #            it
    Int0 = p"[0-9]+"
    IntSuffix = s"'i" + Int0    @ (it => it[1])

    Operators0 = p"[\p{Sm}*/\\?!%&$^@-]+"
    Operators = Operators0                          @ (it => newIdNode(it.fragment).seta(it.pos).setb(it.endpos))

    Pos = pos()
    Fail = Pos @ (it => newFailedNode(newLocation(uri, it, it)))
    NewLine = +(sp(0) > p"\n" @ first) > sp(0)          @ (it => it[^1].fragment.len)
    Indent = NewLine                                    @@ proc(it: PResult[int]): PResult[int] =
                                                            if it.isErr:
                                                                it
                                                            else:
                                                                if indent[^1] < it.get:
                                                                    indent.add it.get
                                                                    it
                                                                else:
                                                                    err it.getSrc, "invalid indentation (Indent)"
    Dedent = alt(
        &Newline                                        @@ proc(it: PResult[int]): PResult[int] =
                                                            if it.isErr:
                                                                it
                                                            else:
                                                                let n = it.get
                                                                if n in indent:
                                                                    if n == indent[^1]:
                                                                        err(it.getSrc, "invalid indentation (Dedent)")
                                                                    # TODO: merge 2 branches
                                                                    elif n == indent[^2]:
                                                                        discard indent.pop
                                                                        # Newline(it.get[0])
                                                                        it
                                                                    else:
                                                                        discard indent.pop
                                                                        it
                                                                else:
                                                                    err(it.getSrc, "invalid indentation (Nodent)"),
        Eof                                             @ (it => indent.pop)
    )
    Nodent = Newline                                    @@ proc(it: PResult[int]): PResult[int] =
                                                            if it.isErr:
                                                                it
                                                            else:
                                                                let n = it.get
                                                                if n == indent[^1]:
                                                                    it
                                                                else:
                                                                    err it.getSrc, "invalid indentation"

    Comment = alt(
        s"#" > sp(0) > p".*"    @ (it => it[1].fragment & it[2].fragment),
        s"#"                    @ (it => "")
    )
    Comments = Pos + separated1(
        Comment,
        Nodent
    ) + Pos @ (it => newCommentNode(it[0][1], newLocation(uri, it[0][0], it[1])))
    Program: AstNode = alt(
        delimited(
            ?Nodent,
            StmtList,
            preceded(?Nodent, Eof)
        ),
        Fail @ (it => (errs.add ParseError(loc: it.loc, msg: @["program"]);it))
    )
    StmtList: AstNode = separated1(
        Statement,
        alt(
            Nodent,
            preceded(!Dedent, Newline + Fail) @ (it => (errs.add ParseError(kind: InvalidIndentation, loc: it[1].loc);it[0]))
        )
    ) @ akStmtList.newTreeNode

    # statement
    Statement: AstNode = alt(
        Comments,
        LetSection,
        VarSection,
        ConstSection,
        TypeSection,
        FuncDef,
        Discard,
        Metadata,
        Asign,
        Expr,
        terminated(Fail, oneline) @ (it => (errs.add ParseError(loc: it.loc, msg: @["statement"]);it))
    )
    IdentDef: AstNode = alt(
        Pattern + alt(
            ?preceded(colon, Expr) +
            preceded(eq, Expr)                      @ ((it: (Option[AstNode], AstNode)) =>
                                                        (
                                                            block:
                                                                let
                                                                    kind = if it[0].isSome:
                                                                        it[0].get
                                                                    else:
                                                                        newEmptyNode()
                                                                    default = it[1]
                                                                @[kind, default]
                                                        )
                                                    ),
            preceded(colon, Expr) +
            ?preceded(eq, Expr)                     @ ((it: (AstNode, Option[AstNode])) =>
                                                        (
                                                            block:
                                                                let
                                                                    kind = it[0]
                                                                    default = if it[1].isSome:
                                                                        it[1].get
                                                                    else:
                                                                        newEmptyNode()
                                                                @[kind, default]
                                                        )
                                                    )
        )                                                   @ (it => akIdentDef.newTreeNode(it[0] & it[1])),
        Pattern
    )
    GenIdentDef: AstNode = alt(
        Pattern + alt(
            ?preceded(colon, Expr) +
            preceded(tlt, Expr)                     @ ((it: (Option[AstNode], AstNode)) =>
                                                        (
                                                            block:
                                                                let
                                                                    kind = if it[0].isSome:
                                                                        it[0].get
                                                                    else:
                                                                        newEmptyNode()
                                                                    default = it[1]
                                                                @[kind, default]
                                                        )
                                                    ),
            preceded(colon, Expr) +
            ?preceded(eq, Expr)                     @ ((it: (AstNode, Option[AstNode])) =>
                                                        (
                                                            block:
                                                                let
                                                                    kind = it[0]
                                                                    default = if it[1].isSome:
                                                                        it[1].get
                                                                    else:
                                                                        newEmptyNode()
                                                                @[kind, default]
                                                        )
                                                    )
        )                                                   @ (it => akIdentDef.newTreeNode(it[0] & it[1])),
        Pattern
    )
    Section = alt(
        preceded(sp1, IdentDef)                 @ boxing,
        delimited(Indent, IdentDef ^+ Nodent, Dedent)
    )
    LetSection = s"let" + Section                       @ (it => akLetSection.newTreeNode(it[1]).seta(it[0].pos))
    VarSection = s"var" + Section                       @ (it => akVarSection.newTreeNode(it[1]).seta(it[0].pos))
    ConstSection = s"const" + Section                   @ (it => akConstSection.newTreeNode(it[1]).seta(it[0].pos))
    TypeSection = s"type" + Section                     @ (it => akConstSection.newTreeNode(it[1]).seta(it[0].pos))
    GenParamList: seq[AstNode] = separated0(
        GenIdentDef,
        comma
    )
    ParamList: seq[AstNode] = separated0(
        IdentDef,
        comma
    )
    GenParams = delimited(
        lbra,
        GenParamList,
        rbra
    )   @ (it => akParams.newTreeNode(it))
    Params: AstNode = delimited(
            lpar,
            ParamList,
            rpar
        ) + ?(preceded(arr, Expr))                      @ proc(it: (seq[AstNode], Option[AstNode])): AstNode =
                                                            let
                                                                rety = if it[1].isSome: it[1].get else: newEmptyNode()
                                                                paramty = it[0]
                                                            akParams.newTreeNode(@[rety] & paramty)
    Suite = alt(
        preceded(
            colon,
            alt(
                Statement @ (it => akStmtList.newTreeNode(@[it])),
                delimited(Indent, StmtList, Dedent),
                terminated(Fail, delimited(Indent, oneline ^* Nodent, Dedent)) @ (it => (errs.add ParseError(loc: it.loc, msg: @["expression"]); it)),
                terminated(Fail, alt(&Nodent, Eof @ (it => 0), Dedent)) @ (it => (errs.add ParseError(loc: it.loc, msg: @["expression"]); it))
            )
        ),
        preceded(
            sp0,
            Fail + alt(
                Statement @ (it => akStmtList.newTreeNode(@[it])),
                delimited(Indent, StmtList, Dedent),
                terminated(Fail, delimited(Indent, oneline ^* Nodent, Dedent)),
                terminated(Fail, alt(&Nodent, Eof @ (it => 0), Dedent))
            ) @ (it => (errs.add ParseError(loc: it[0].loc, msg: @["\":\""]);it[1]))
        ),
    )
    NoBody = sp0 + &NewLine @ proc(it: auto): AstNode = newEmptyNode()
    FuncDef: AstNode = preceded(
        fun > sp1,
        Id + ?GenParams + Params + ?Metadata + alt(NoBody, Suite)
    )   @ proc(it: ((((AstNode, Option[AstNode]), AstNode), Option[AstNode]), AstNode)): AstNode =
            let
                id = it[0][0][0][0]
                genparams = if it[0][0][0][1].isSome: it[0][0][0][1].get else: newEmptyNode()
                params = it[0][0][1]
                meta = if it[0][1].isSome: it[0][1].get else: newEmptyNode()
                # body = if it[1][1].isSome: it[1][1].get else: newEmptyNode()
                body = it[1]
            akFuncDef.newTreeNode(@[id, genparams, params, meta, body])

    # epression
    asop = p"[\p{Sm}*/\\?!%&$^@-]*="                    @ (it => newIdNode(it.fragment))
    Asign: AstNode = Expr > asop ^ sp0 > Expr           @ akAsign.newTreeNode
    Expr: AstNode = alt(
        IfExpr,
        WhenExpr,
        LoopExpr,
        LambdaDef,
        # ForExpr,
        SimplExpr
    )
    CondBranch = alt(
        terminated(Expr, sp0) > Suite,
        preceded(sp0, Fail > Suite) @ (it => (errs.add ParseError(loc: it[0].loc, msg: @["expression"]); it))
    )
    ElifBranch = preceded(elift > sp1, CondBranch)                      @ (it => akElifBranch.newTreeNode(it))
    ElseBranch = preceded(terminated(elset, sp0), Suite)                @ (it => akElseBranch.newTreeNode(@[it]))

    IfExpr = (preceded(
        ift > sp1,
        CondBranch @ (it => akElifBranch.newTreeNode(it))
    ) > *preceded(Nodent, ElifBranch)) + ?preceded(Nodent, ElseBranch) @ (it => akIfExpr.newTreeNode(it[0] & (if it[1].isSome: @[it[1].get] else: @[])))

    WhenExpr = (preceded(
        whent > sp1,
        CondBranch @ (it => akElifBranch.newTreeNode(it))
    ) > *preceded(Nodent, ElifBranch)) + ?preceded(Nodent, ElseBranch) @ (it => akWhenExpr.newTreeNode(it[0] & (if it[1].isSome: @[it[1].get] else: @[])))

    LoopExpr = preceded(
        loop > colon,
        alt(
            delimited(
                Indent,
                StmtList,
                Dedent
            ),
            Statement @ boxing @ akStmtList.newTreeNode
        )
    )                                                   @ boxing @ akLoopExpr.newTreeNode
    LambdaDef: AstNode = preceded(
        s"lambda" + sp1,
        terminated(Id, sp0 + colon) > SimplExpr
    )                                                   @ (it => akLambdaDef.newTreeNode(it))

    SimplExpr: AstNode = alt(
        ArrowExpr,
        # ArrowExpr > Suite                               @ (it => akCall.newTreeNode(it))
    )

    AtomExpr: AstNode = Atom + *Trailer                 @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (kind, children) in it[1]:
                                                                result = kind.newTreeNode(result & children)
    AtomExprNotCommand: AstNode = Atom + *TrailerNotCommand @ proc(it: auto): auto =
                                                                result = it[0]
                                                                for (kind, children) in it[1]:
                                                                    result = kind.newTreeNode(result & children)
    ArgList: seq[AstNode] = separated0(
        alt(
            AtomExprNotCommand >
            preceded(sp1 + !Operators, Expr)            @ akCommand.newTreeNode,
            Expr,
        ),
        comma
    )
    ArgList1: seq[AstNode] = separated1(
        alt(
            AtomExprNotCommand >
            preceded(sp1 + !Operators, Expr)            @ akCommand.newTreeNode,
            Expr,
        ),
        comma
    )
    Trailer: (AstKind, seq[AstNode]) = alt(
        delimited(!sp1, Operators, !(Atom ^ sp0))       @ (it => (akPostfix, @[it])),
        preceded(dot, Int)                       @ (it => (akDot, @[it])),
        preceded(dot, Atom)                       @ (it => (akDot, @[it])),
        preceded(sp1 + !terminated(Operators, sp1), ArgList1) @ (it => (akCommand, it)),
        delimited(lpar, ArgList, rpar)                  @ (it => (akCall, it)),
        delimited(lbra, ArgList, rbra)                  @ (it => (akBracketExpr, it)),
    )
    TrailerNotCommand: (AstKind, seq[AstNode]) = alt(
        delimited(!sp1, Operators, !(Atom ^ sp0))       @ (it => (akPostfix, @[it])),
        preceded(dot, Atom)                       @ (it => (akDot, @[it])),
        delimited(lpar, ArgList, rpar)                  @ (it => (akCall, it)),
        delimited(lbra, ArgList, rbra)                  @ (it => (akBracketExpr, it)),
    )

    PowerExpr = AtomExpr + *(powerop ^ sp0 + AtomExpr)  @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                result = akInfix.newTreeNode(@[op, result, right])
    MulExpr = PowerExpr + *(mulop ^ sp0 + PowerExpr)    @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                result = akInfix.newTreeNode(@[op, result, right])
    PlusExpr = MulExpr + *(plusop ^ sp0 + MulExpr)      @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                result = akInfix.newTreeNode(@[op, result, right])
    AmpExpr = PlusExpr + *(ampop ^ sp0 + PlusExpr)      @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                result = akInfix.newTreeNode(@[op, result, right])
    RangeExpr = AmpExpr + *(rangeop ^ sp0 + AmpExpr)    @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                result = akInfix.newTreeNode(@[op, result, right])
    CmpExpr = RangeExpr + *(cmpop ^ sp0 + RangeExpr)    @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                result = akInfix.newTreeNode(@[op, result, right])
    AndExpr = CmpExpr + *(andop ^ sp0 + CmpExpr)        @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                result = akInfix.newTreeNode(@[op, result, right])
    OrExpr = AndExpr + *(orop ^ sp0 + AndExpr)          @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                result = akInfix.newTreeNode(@[op, result, right])
    SymbolExpr = OrExpr + *(symbolop ^ sp0 + OrExpr)    @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                result = akInfix.newTreeNode(@[op, result, right])
    AsgnExpr = SymbolExpr + *(asgnop ^ sp0 + SymbolExpr)    @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                result = akInfix.newTreeNode(@[op, result, right])
    ArrowExpr = AsgnExpr + *(arrowop ^ sp0 + AsgnExpr)  @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                result = akInfix.newTreeNode(@[op, result, right])

    # atom
    Atom: AstNode = ?Operators + alt(
        Literal, Id,
        delimited(lpar, Expr, rpar),
        Tuple,
        Record
    )                                                   @ proc(it: (Option[AstNode], AstNode)): auto =
                                                            let (op, atom) = it
                                                            if op.isSome:
                                                                akPrefix.newTreeNode(@[op.get, atom])
                                                            else:
                                                                atom


    Id = Id0                                            @ (it => newIdNode(it.fragment, it.toLocation(uri)))
    Int = alt(
        Int0 + IntSuffix    @ (it => newIntNode(parseInt(it[0].fragment), it[1].fragment.parseInt.uint, newLocation(uri, it[0], it[1]))),
        Int0                @ (it => newIntNode(parseInt(it.fragment), 0, it.toLocation(uri))),
    )
    Float = alt(
        Int0 > dot > Int0                               @ (it => newFloatNode(parseFloat(it[0].fragment & "." & it[2].fragment), newLocation(uri, it[0], it[2]))),
        Int0 > dot > !Id0                               @ (it => newFloatNode(parseFloat(it[0].fragment), newLocation(uri, it[0], it[1]))),
        dot > Int0                                      @ (it => newFloatNode(parseFloat("." & it[1].fragment), newLocation(uri, it[0], it[1])))
    )
    Boolean = alt(
        falset          @ (it => newBoolNode(false, newLocation(uri, it, it))),
        truet           @ (it => newBoolNode(true, newLocation(uri, it, it)))
    )
    String = strlit     @ (it => newStrNode(it.fragment[1..^2], it.toLocation(uri)))
    Char = charlit      @ (it => newCharNode(it.fragment[1], it.toLocation(uri)))
    Literal = alt(
        Float,
        Int,
        Boolean,
        String,
        Char,
    )
    Tuple = delimited(lpar, Expr^*(comma), ?comma+rpar)        @ (it => akTuple.newTreeNode(it))
    Record = delimited(lpar, (Id + ?(preceded(colon, Expr)))^*comma, ?comma+rpar) @ (proc(a: auto): auto =
                                                                                        let ch = a.mapIt(akIdentDef.newTreeNode(if it[1].isSome: @[it[0], it[1].get] else: @[it[0]]))
                                                                                        akRecord.newTreeNode(ch)
                                                                                    )
    DiscardPattern = us                                 @ (it => akDiscardPattern.newNode())
    IdPattern = Id
    LiteralPattern = Literal
    TuplePattern: AstNode = delimited(lpar, Pattern^*(comma), ?comma+rpar)              @ (it => akTuple.newTreeNode(it))
    RecordPattern: AstNode = delimited(
        lpar,
        (Id + ?(preceded(colon, Pattern)))^*comma,
        ?comma+rpar
    )   @ (proc(a: auto): auto =
            let ch = a.mapIt(akIdentDef.newTreeNode(if it[1].isSome: @[it[0], it[1].get] else: @[it[0]]))
            akRecord.newTreeNode(ch)
        )
    BracketPattern: AstNode = IdPattern > delimited(lbra, separated1(Pattern, comma), rbra)    @ (it => akBracketExpr.newTreeNode(it))

    Pattern = alt(
        DiscardPattern,
        IdPattern,
        LiteralPattern,
        TuplePattern,
        RecordPattern,
        BracketPattern,
    )
    Metadata = preceded(
        bikkuri,
        delimited(
            lbra,
            Id + ?preceded(colon, Literal),
            rbra
        )                                               @ (it => (if it[1].isSome: akMetadata.newTreeNode(@[it[0], it[1].get]) else: akMetadata.newTreeNode(@[it[0]])))
    )
    Discard = alt(
        preceded(
            discar + sp1,
            Expr
        )                                               @ (it => akDiscard.newTreeNode(@[it])),
        discar                                          @ (it => akDiscard.newNode()),
    )

export newParser, `$`
proc parse*(self: ref Parser, filename: string): AstNode =
    self.indent = @[0]
    self.uri = parseUri(filename)
    self.errs = @[]

    let s = open(filename, fmRead)
    defer:
        s.close()
    let ret = self.Program(s.readAll)
    ret.unwrap
proc parse*(self: ref Parser, filename: string, text: string): AstNode =
    self.indent = @[0]
    self.uri = parseUri(filename)
    self.errs = @[]

    let ret = self.Program(text)
    ret.unwrap
proc errs*(self: ref Parser): seq[ParseError] = self.errs


when isMainModule:
    var
        parser = newParser()
    let a = r"""
let
    a: int = 3
    b: int = 3

    c: float = 3.5

    d: float


    e = 4.005


let å = 3
let
    b=3
var
    a: int = 3
    b: int = 3

    c: float = 3.5

    d: float


    e = 4.005
alias
    a = 3
    `∆` = 2

var å = 3
var
    b=3

3

3-> 3 * 3 - 3

a = 3
![a]
![a: 3]
![a: "arith.ll"]
# func fact(a):
#     a * fact(a-1)
false
"""
    let b = """

for e in a:
    echo e

"""
    # var res = parser.Program(a)
    # if res.isOk:
    #     echo res.get
    # else:
    #     echo res
    # var res = parser.Program(b)
    # if res.isOk:
    #     echo res.get
    # else:
    #     echo res
    #     echo res.src
    let c = "_, a, 3"
    var pat = parser.Program(a)
    if pat.isOk:
        echo pat.get
    else:
        echo pat
        echo pat.src
