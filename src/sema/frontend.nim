
import sequtils

import macros
import ast_pattern_matching

import ir
import typeenvs

import ../lineinfos


proc parseImpl(body: NimNode, scope: NimNode = newNilLit()): NimNode =
    let
        newScope = newIdentNode("scope")
        newScopeProc = bindSym("newScope")
        newLocationProc = bindSym("newLocation")
    matchAst body:
    of {nnkStmtList, nnkTypeSection}:
        let
            s = nnkPrefix.newTree(
                newIdentNode("@"),
                nnkBracket.newTree(
                    toSeq(body.children).mapIt(it.parseImpl(newScope))
                )
            )
        result = quote:
            block:
                let
                    `newScope` = `newScopeProc`(`scope`)
                    e = Expression.Seq(`s`, `newLocationProc`())
                e.scope = `newScope`
                e
    of nnkIntLit:
        result = quote:
            Expression.Lit(Literal.Integer(`body`, 0), newLocation())
    of nnkFloatLit:
        result = quote:
            Expression.Lit(Literal.Float(`body`, 0), newLocation())
    of nnkCall(ident"typeof", `e`@_):
        let
            exp = e.parseImpl(scope)
        result = quote:
            Expression.Typeof(`exp`, newLocation())
    of nnkTypeDef:
        let
            name = newLit body[0].strVal
            typ = body[2].parseImpl(newScope)
        result = quote:
            block:
                let
                    `newScope` = `newScopeProc`(`scope`)
                    typedef = TypeDef(
                        ident: newIdent(`name`, newLocation()),
                        typ: TypeExpression.Expr(`typ`)
                    )
                    e = Expression.TypeSection(typedef, newLocation())
                e.scope = `newScope`
                e
    else:
        result = quote:
            Expression.Lit(Literal.Unit, newLocation())

macro parse(body: untyped): untyped =
    result = body.parseImpl()
    echo result.treeRepr
    echo result.repr


when isMainModule:
    dumpTree:
        type
            int = typeof(0)
            float = typeof(0.0)
            Pair[A, B] = (A, B)
    let program = parse:
        type
            int = typeof(0)
            float = typeof(0.0)
        proc id(a: int): int =
            a
        0
    echo program
