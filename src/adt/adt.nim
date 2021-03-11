
import sequtils
import strformat

import macros
import macroutils
import ast_pattern_matching


type
    Variant = object

proc scanId(body: NimNode): (bool, NimNode, NimNode) =
    body.matchAst:
    of nnkIdent:
        result = (false, body, newEmptyNode())
    of nnkBracketExpr:
        let
            name = body[0]
            generics = body[1..^1]
        name.expectKind(nnkIdent)
        for e in generics:
            e.expectKind(nnkIdent)
        result = (true, name, GenericParams(generics.mapIt(IdentDefs(it, newEmptyNode(), newEmptyNode()))))

proc genAdt(name, body: NimNode): NimNode =
    let
        (isGeneric, name, generics) = scanId(name)
    var
        kinds: seq[NimNode]
        kindId = ident(name.strVal & "kind'")
        adtDefRecCase = nnkRecCase.newTree(IdentDefs("kind", kindId, newEmptyNode()))
    proc enBracket(id: NimNode): NimNode =
        if isGeneric:
            nnkBracketExpr.newTree(id).add(
                generics.mapIt(it[0])
            )
        else:
            id
    result = nnkTypeSection.newTree()
    for e in body:
        e.matchAst:
        of nnkIdent:
            kinds.add e
            adtDefRecCase.add OfBranch(@[DotExpr(kindId, e)], newNilLit())
        of nnkObjConstr:
            let
                kind = e[0]
                args = e[1..^1]
                id = ident(fmt"{kind.strVal}'")
            kinds.add kind
            for e in args:
                e.expectKind(nnkExprColonExpr)
            result.add TypeDef(
                id,
                generics,
                TupleTy(
                    args.mapIt(IdentDefs(it[0], it[1], newEmptyNode()))
                )
            )
            adtDefRecCase.add OfBranch(@[DotExpr(kindId, kind)], RecList(IdentDefs(fmt"{kind.strVal}impl", enBracket(id), newEmptyNode())))

    result.add newEnum(kindId, kinds, false, true)[0]
    result.add TypeDef(
        name, generics,
        nnkObjectTy.newTree(
            newEmptyNode(), newEmptyNode(),
            RecList(adtDefRecCase)
        )
    )

proc adtImpl(name, body: NimNode): NimNode =
    genAdt(name, body)

macro Adt*(body: untyped): untyped =
    result = newStmtList(
        body.mapIt(
            adtImpl(it[0], it[1])
        )
    )
    echo result.repr

dumpTree:
    type
        A[T] = object
            case kind: A
            of a:
                a: b[T, A]
            of b:
                c: d

when isMainModule:
    Adt:
        Option[T]:
            Some(val: T)
            None