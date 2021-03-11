
import strutils
import options
import sugar

import ../parserCombinator except s, p, Parser
import ../ast
import ../lineInfos

import ../parserCombinator/dsl


proc first[T](a: seq[T]): T = a[0]
proc boxing[T](a: T): seq[T] = @[a]
proc newTreeNode(kind: range[akFailed..akCall]): proc(a: seq[AstNode]): AstNode =
    result = proc(a: seq[AstNode]): AstNode =
        kind.newTreeNode(a)

ParserDef Parser[Spanned](fileid: FileId, indent: seq[int]):
    fileid = -1
    indent = @[0]

    Eof = !p"."

    Id0 = alt(
        p"[_\p{L}\p{Nl}ー][_\p{L}\p{N}ー]*",
        -s"`" > p"[_\p{L}\p{Sm}\p{Nl}ー][_\p{L}\p{Sm}\p{N}ー]*" > -s"`" @ first
    )
    Int0 = p"[0-9]+"

    Operators = p"[\p{Sm}*/\\?!%&$^@]+"
    KeyWords = p"let|var|const"

    NewLine = - +(sp(0) > p"\n" @ first) > sp(0)        @ first @ (it => it.fragment.len)
    Indent = NewLine                                    @@ proc(it: Result[Spanned, int, string]): Result[Spanned, int, string] =
                                                            if it.isErr:
                                                                it
                                                            else:
                                                                if indent[^1] < it.get[1]:
                                                                    indent.add it.get[1]
                                                                    it
                                                                else:
                                                                    err "invalid indentation"
    Dedent = alt(
        &Newline                                        @@ proc(it: Result[Spanned, int, string]): Result[Spanned, int, string] =
                                                            if it.isErr:
                                                                it
                                                            else:
                                                                let n = it.get[1]
                                                                if n in indent:
                                                                    if n == indent[^1]:
                                                                        err "invalid indentation"
                                                                    # TODO: merge 2 branches
                                                                    elif n == indent[^2]:
                                                                        discard indent.pop
                                                                        # Newline.parse(it.get[0])
                                                                        it
                                                                    else:
                                                                        discard indent.pop
                                                                        it
                                                                else:
                                                                    err "invalid indentation"
        ,
        Eof                                            @ (it => indent.pop)
    )
    Nodent = Newline                                    @@ proc(it: Result[Spanned, int, string]): Result[Spanned, int, string] =
                                                            if it.isErr:
                                                                it
                                                            else:
                                                                let n = it.get[1]
                                                                if n == indent[^1]:
                                                                    it
                                                                else:
                                                                    err "invalid indentation"

    Program: AstNode = Statement ^* Nodent              @ akStmtList.newTreeNode
    #                  TODO: consider about the term:
    #                           terminated(Statement ^* Nodent, Eof)
    Statement: AstNode = LetSection \ Asign \ Expr
    IdentDef: AstNode = Id + alt(
        ?preceded(s":" ^ sp(0), Expr) +
        preceded(s"=" ^ sp(0), Expr)            @ ((it: (Option[AstNode], AstNode)) =>
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
        preceded(s":" ^ sp(0), Expr) +
        ?preceded(s"=" ^ sp(0), Expr)           @ ((it: (AstNode, Option[AstNode])) =>
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
    LetSection = s"let" + alt(
        preceded(sp(1), IdentDef)               @ boxing,
        delimited(Indent, IdentDef ^+ Nodent, Dedent)
    )                                                   @ (it => akLetSection.newTreeNode(it[1]).seta(it[0].pos))
    asop = p"[\p{Sm}*/\\?!%&$^@]*="                     @ (it => newIdNode(it.fragment))
    Asign: AstNode = Expr > asop ^ sp(0) > Expr         @ akAsign.newTreeNode
    Expr: AstNode = Atom
    Atom: AstNode = Float \ Int \ Id

    Id = Id0                                            @ (it => newIdNode(it.fragment, it.toLineInfo(fileid)))
    Int = Int0                                          @ (it => newIntNode(parseInt(it.fragment), it.toLineInfo(fileid)))
    Float = alt(
        Int0 > -s"." > Int0                             @ (it => newFloatNode(parseFloat(it[0].fragment & "." & it[1].fragment), newLineInfo(fileid, it[0], it[1]))),
        Int0 > s"." > !Id0                              @ (it => newFloatNode(parseFloat(it[0].fragment), newLineInfo(fileid, it[0], it[1]))),
        s"." > Int0                                     @ (it => newFloatNode(parseFloat("." & it[1].fragment), newLineInfo(fileid, it[0], it[1])))
    )

proc parse(self: Parser, filename: string): AstNode =
    self.indent = @[0]
    self.fileid = newFileId(filename)

    let s = open(filename, fmRead)
    defer:
        s.close()
    self.Program.parse(s.readAll).unwrap


when isMainModule:
    var
        parser = newParser()
    echo parser.Id.parse("a_u_i_e").unwrap()
    echo parser.Id.parse("日本語の変数エー3").unwrap()
    echo parser.Id.parse("aiueo").unwrap()
    echo parser.Operators.parse("∇⨕⫅=+*/\\?!~%&$|^@").unwrap()
    echo parser.Int.parse("345").unwrap()
    echo parser.Float.parse("123.345").unwrap()
    echo parser.Atom.parse("123.345").unwrap()
    echo parser.Atom.parse("123.").unwrap()
    echo parser.Atom.parse("123.abc").unwrap()
    echo parser.Atom.parse(".009").unwrap()
    echo parser.Atom.parse("123").unwrap()
    echo parser.Asign.parse("3 *= 3").unwrap()
    echo parser.LetSection.parse("let a: int = 3").unwrap()
    let a = """let
    a: int = 3
    b: int = 3

    c: float = 3.5

    d: float


    e = 4.005


let å = 3
let
    b=3
a = 3
3"""
    echo parser.Program.parse(a).unwrap()