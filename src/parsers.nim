
import strutils
import options
import sugar

import eat except Parser
import eat/dsl

import ast
import lineInfos



proc first[T](a: seq[T]): T = a[0]
proc second[T](a: seq[T]): T = a[1]
proc boxing[T](a: T): seq[T] = @[a]
proc newTreeNode(kind: range[akFailed..akCall]): proc(a: seq[AstNode]): AstNode =
    result = proc(a: seq[AstNode]): AstNode =
        kind.newTreeNode(a)

ParserDef Parser(fileid: FileId, indent: seq[int]):
    fileid = -1
    indent = @[0]

    Eof = !p".|\n|\r\n|\r"

    sp0     = sp(0)
    sp1     = sp(1)
    fun     = s"func"
    ift     = s"if"
    whent   = s"when"
    elift   = s"elif"
    elset   = s"else"
    fort    = s"for"
    intt    = s"in"
    whilet  = s"while"
    loop    = s"loop"
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
        s"`" > p"[_\p{L}\p{Sm}\p{Nl}ー][_\p{L}\p{Sm}\p{N}ー]*" > s"`" @ second
    )                                                   #@@ proc(it: PResult[Spanned]): PResult[Spanned] =
                                                        #    if it.isErr:
                                                        #        it
                                                        #    else:
                                                        #        if it.get.fragment in ["func", "if", "elif", "let", "var", "const"]:
                                                        #            err(it.getSrc, "keyword")
                                                        #        else:
                                                        #            it
    Int0 = p"[0-9]+"

    Operators = p"[\p{Sm}*/\\?!%&$^@-]+"                @ (it => newIdNode(it.fragment).seta(it.pos).setb(it.endpos))

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

    Comment = s"#" > sp(0) > p".*"                      @ (it => akComment.newTreeNode(@[newStrNode(it[2].fragment)]))
    Program: AstNode = delimited(
        ?Nodent,
        StmtList,
        preceded(?Nodent, Eof)
    )
    StmtList: AstNode = separated1(Statement, Nodent)   @ akStmtList.newTreeNode
    
    # statement
    Statement: AstNode = alt(
        Comment,
        LetSection,
        VarSection,
        ConstSection,
        AliasSection,
        FuncDef,
        Asign,
        Expr,
        Metadata
    )
    IdentDef: AstNode = Patterns + alt(
        ?preceded(colon ^ sp0, Expr) +
        preceded(eq ^ sp0, Expr)            @ ((it: (Option[AstNode], AstNode)) =>
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
        preceded(colon ^ sp0, Expr) +
        ?preceded(eq ^ sp0, Expr)           @ ((it: (AstNode, Option[AstNode])) =>
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
    )                                                   @ (it => akIdentDef.newTreeNode(it[0] & it[1]))
    Section = alt(
        preceded(sp1, IdentDef)                 @ boxing,
        delimited(Indent, IdentDef ^+ Nodent, Dedent)
    )
    LetSection = s"let" + Section                       @ (it => akLetSection.newTreeNode(it[1]).seta(it[0].pos))
    VarSection = s"var" + Section                       @ (it => akVarSection.newTreeNode(it[1]).seta(it[0].pos))
    ConstSection = s"const" + Section                   @ (it => akConstSection.newTreeNode(it[1]).seta(it[0].pos))
    AliasSection = s"alias" + Section                   @ (it => akAliasSection.newTreeNode(it[1]).seta(it[0].pos))
    ParamList: seq[AstNode] = separated0(
        Id,
        comma
    )
    FuncDef: AstNode = preceded(
        fun > sp1,
        terminated(
            Id,
            delimited(
                lpar ^ sp0,
                ParamList,
                rpar ^ sp0
            )
        )
    ) > delimited(colon + Indent, StmtList, Dedent)     @ akFuncDef.newTreeNode

    # epression
    asop = p"[\p{Sm}*/\\?!%&$^@-]*="                    @ (it => newIdNode(it.fragment))
    Asign: AstNode = Expr > asop ^ sp0 > Expr           @ akAsign.newTreeNode
    Expr: AstNode = alt(
        IfExpr,
        WhenExpr,
        LoopExpr,
        # ForExpr,
        SimplExpr
    )
    CondBranch = terminated(Expr, colon) > alt(
        delimited(
            Indent,
            StmtList,
            Dedent
        ),
        Statement @ (it => akStmtList.newTreeNode(@[it]))
    )
    ElifBranch = preceded(elift > sp1, CondBranch)      @ (it => akElifBranch.newTreeNode(it))
    ElseBranch = preceded(
        elset > colon ^ sp0,
        alt(
            delimited(
                Indent,
                StmtList,
                Dedent
            ),
            Statement  @ (it => akStmtList.newTreeNode(@[it]))
        )
    )                                                   @ (it => akElseBranch.newTreeNode(@[it]))

    IfExpr = (preceded(
        ift > sp1,
        CondBranch @ (it => akElifBranch.newTreeNode(it))
    ) > *preceded(Nodent, ElifBranch)) + ?preceded(Nodent, ElseBranch) @ (it => akIfExpr.newTreeNode(it[0] & (if it[1].isSome: @[it[1].get] else: @[])))

    WhenExpr = (preceded(
        whent > sp1,
        CondBranch @ (it => akElifBranch.newTreeNode(it))
    ) > *preceded(Nodent, ElifBranch)) + ?preceded(Nodent, ElseBranch) @ (it => akWhenExpr.newTreeNode(it[0] & (if it[1].isSome: @[it[1].get] else: @[])))

    LoopExpr = preceded(
        loop > colon ^ sp0, 
        alt(
            delimited(
                Indent,
                StmtList,
                Dedent
            ),
            Statement @ boxing @ akStmtList.newTreeNode
        )
    )                                                   @ boxing @ akLoopExpr.newTreeNode

    SimplExpr: AstNode = ArrowExpr

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
        preceded(dot ^ sp0, Atom)                       @ (it => (akDot, @[it])),
        preceded(sp1 + !Operators, ArgList1)            @ (it => (akCommand, it)),
        delimited(lpar, ArgList, rpar)                  @ (it => (akCall, it)),
    )
    TrailerNotCommand: (AstKind, seq[AstNode]) = alt(
        delimited(!sp1, Operators, !(Atom ^ sp0))       @ (it => (akPostfix, @[it])),
        preceded(dot ^ sp0, Atom)                       @ (it => (akDot, @[it])),
        delimited(lpar, ArgList, rpar)                  @ (it => (akCall, it)),
    )

    PowerExpr = AtomExpr + *(powerop ^ sp0 + AtomExpr)  @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                echo "power: ", op.strVal
                                                                result = akInfix.newTreeNode(@[op, result, right])             
    MulExpr = PowerExpr + *(mulop ^ sp0 + PowerExpr)    @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                echo "mul: ", op.strVal
                                                                result = akInfix.newTreeNode(@[op, result, right])             
    PlusExpr = MulExpr + *(plusop ^ sp0 + MulExpr)      @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                echo "plus: ", op.strVal
                                                                result = akInfix.newTreeNode(@[op, result, right])             
    AmpExpr = PlusExpr + *(ampop ^ sp0 + PlusExpr)      @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                echo "amp: ", op.strVal
                                                                result = akInfix.newTreeNode(@[op, result, right])             
    RangeExpr = AmpExpr + *(rangeop ^ sp0 + AmpExpr)    @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                echo "ramge: ", op.strVal
                                                                result = akInfix.newTreeNode(@[op, result, right])             
    CmpExpr = RangeExpr + *(cmpop ^ sp0 + RangeExpr)    @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                echo "cmp: ", op.strVal
                                                                result = akInfix.newTreeNode(@[op, result, right])             
    AndExpr = CmpExpr + *(andop ^ sp0 + CmpExpr)        @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                echo "and: ", op.strVal
                                                                result = akInfix.newTreeNode(@[op, result, right])             
    OrExpr = AndExpr + *(orop ^ sp0 + AndExpr)          @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                echo "or: ", op.strVal
                                                                result = akInfix.newTreeNode(@[op, result, right])             
    SymbolExpr = OrExpr + *(symbolop ^ sp0 + OrExpr)    @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                echo "symbol: ", op.strVal
                                                                result = akInfix.newTreeNode(@[op, result, right])             
    AsgnExpr = SymbolExpr + *(asgnop ^ sp0 + SymbolExpr)    @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                echo "asgn: ", op.strVal
                                                                result = akInfix.newTreeNode(@[op, result, right])             
    ArrowExpr = AsgnExpr + *(arrowop ^ sp0 + AsgnExpr)  @ proc(it: auto): auto =
                                                            result = it[0]
                                                            for (op, right) in it[1]:
                                                                echo "arrow: ", op.strVal
                                                                result = akInfix.newTreeNode(@[op, result, right])

    # atom
    Atom: AstNode = ?Operators + alt(
        Literal, Id,
        delimited(lpar, Expr, rpar)
    )                                                   @ proc(it: (Option[AstNode], AstNode)): auto =
                                                            let (op, atom) = it
                                                            if op.isSome:
                                                                akPrefix.newTreeNode(@[op.get, atom])
                                                            else:
                                                                atom


    Id = Id0                                            @ (it => newIdNode(it.fragment, it.toLineInfo(fileid)))
    Int = Int0                                          @ (it => newIntNode(parseInt(it.fragment), it.toLineInfo(fileid)))
    Float = alt(
        Int0 > dot > Int0                               @ (it => newFloatNode(parseFloat(it[0].fragment & "." & it[2].fragment), newLineInfo(fileid, it[0], it[2]))),
        Int0 > dot > !Id0                               @ (it => newFloatNode(parseFloat(it[0].fragment), newLineInfo(fileid, it[0], it[1]))),
        dot > Int0                                      @ (it => newFloatNode(parseFloat("." & it[1].fragment), newLineInfo(fileid, it[0], it[1])))
    )
    # String = s("\"") > s("\"")                          @ (it => newStrNode())
    Literal = alt(
        Float,
        Int,
    )
    DiscardPattern = us                                 @ (it => akDiscardPattern.newNode())
    IdPattern = Id                                      @ (it => akIdPattern.newTreeNode(@[it]))
    LiteralPattern = Literal                            @ (it => akLiteralPattern.newTreeNode(@[it]))
    # TuplePattern

    Patterns = alt(
        DiscardPattern,
        IdPattern,
        LiteralPattern
    ) ^+ comma                                          @ (it => akPatterns.newTreeNode(it))
    Metadata = preceded(
        bikkuri,
        delimited(lbra, Id, rbra)                       @ (it => akMetadata.newTreeNode(@[it]))
    )

export newParser, `$`
proc parse*(self: Parser, filename: string): AstNode =
    self.indent = @[0]
    self.fileid = newFileId(filename)

    let s = open(filename, fmRead)
    defer:
        s.close()
    self.Program(s.readAll).unwrap


when isMainModule:
    var
        parser = newParser()
    let a = """
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

# func fact(a):
#     a * fact(a-1)
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
