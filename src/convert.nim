
import sequtils
import options

import ast
import il


proc newPattern*(n: AstNode): Pattern =
    case n.kind
    of akId:
        Pattern.Id(Term.Id(n.strVal))
    else:
        echo n.kind
        echo n
        assert false, "notimplemented"
        Pattern()

proc newTerm*(n: AstNode, scope: Scope): ref Term =
    result = case n.kind
    of akEmpty:
        Term.Unit
    of akStmtList:
        Term.Seq(n.children.mapIt(it.newTerm(scope)))
    of akLetSection:
        let ts = n.children.mapIt(
            block:
                assert it.kind == akIdentDef
                assert it.children.len == 3
                let
                    aid = it.children[0]
                    typ = it.children[1]
                    default = it.children[2]
                assert aid.kind == akId, ""
                let id = newTerm(aid, scope)
                assert not default.isNil, "let section needs initialization"
                if typ.isEmpty():
                    Term.Let(newIdentDef(id, default=newTerm(default, scope)))
                else:
                    Term.Let(newIdentDef(id, newTerm(typ, scope), newTerm(default, scope)))
        )
        Term.Seq(ts)
    of akVarSection:
        let ts = n.children.mapIt(
            block:
                assert it.kind == akIdentDef
                assert it.children.len == 3
                let
                    aid = it.children[0]
                    typ = it.children[1]
                    default = it.children[2]
                assert aid.kind == akId, ""
                let id = newTerm(aid, scope)
                assert not default.isNil, "let section needs initialization"
                if typ.isEmpty():
                    Term.Var(newIdentDef(id, default=newTerm(default, scope)))
                else:
                    Term.Var(newIdentDef(id, newTerm(typ, scope), newTerm(default, scope)))

        )
        Term.Seq(ts)
    of akAliasSection:
        let ts = n.children.mapIt(
            block:
                assert it.kind == akIdentDef
                assert it.children.len == 3
                let
                    aid = it.children[0]
                    typ = it.children[1]
                    default = it.children[2]
                assert aid.kind == akId, ""
                let id = newTerm(aid, scope)
                assert typ.isEmpty(), "notimplemented type annotation"
                assert not default.isNil, "alias section needs initialization"
                newIdentDef(id, default=newTerm(default, scope))
        )
        Term.Typedef(ts)
    of akFuncDef:
        let
            fname = n.children[0]
            funty = n.children[1]
            rety = funty.children[0]
            paramty = funty.children[1..^1]
            metadata = n.children[2]
            body = n.children[3]
            scope = newScope(scope)
        assert fname.kind == akId
        assert rety.kind in @[akId, akEmpty]
        for e in paramty:
            assert e.children[0].kind == akId
            assert e.children[2].isEmpty, "default value is not supported"
        let
            meta = if metadata.isEmpty: none Metadata else: some newTerm(metadata, scope).metadata
            params = paramty.mapIt(IdentDef(id: newTerm(it.children[0], scope), typ: some newTerm(it.children[1], scope)))
        if meta.isSome and meta.get.kind == MetadataKind.ImportLL:
            assert body.isEmpty
        else:
            assert not body.isEmpty
        # let fn = newFunction(fname.strVal, params, newTerm(rety), newTerm(body), meta)
        let fn = Function(id: newTerm(fname, scope), param: FunctionParam(params: params, rety: newTerm(rety, scope)), body: newBody(newTerm(body, scope), scope), metadata: meta)
        Term.FuncDef(fn)
    of akAsign:
        let
            pat = newTerm(n.children[0], scope)
            val = newTerm(n.children[2], scope)
        Term.Asign(pat, val)
    of akMetadata:
        let
            name = n.children[0]
            param = if n.children.len == 1: nil else: newTerm(n.children[1], scope)
        assert name.kind == akId
        let metadata = case name.strVal
        of "link":
            Metadata.Link(param)
        of "importll":
            Metadata.ImportLL(param)
        of "subtype":
            Metadata.Subtype
        else:
            Metadata.UserDef(name.strVal, param)
        Term.Meta(metadata)
    of akDiscard:
        assert n.children.len < 2
        let a = if n.children.len == 1:
            newTerm(n.children[0], scope)
        else:
            Term.Unit
        Term.Discard(a)
    of akIfExpr:
        if n.children[^1].kind == akElseBranch:
            Term.If(
                n.children[0..^2].mapIt(
                    (
                        newTerm(it.children[0], scope),
                        block:
                            let scope = newScope(scope)
                            newBody(newTerm(it.children[1], scope), scope)
                    )
                ),
                block:
                    let scope = newScope(scope)
                    newBody(newTerm(n.children[^1].children[0], scope), scope)
            )
        else:
            Term.If(
                n.children[0..^1].mapIt(
                    (newTerm(it.children[0], scope),
                    block:
                        let scope = newScope(scope)
                        newBody(newTerm(it.children[1], scope), scope))
                ),
                block:
                    let scope = newScope(scope)
                    newBody(Term.Unit, scope)
            )
    of akLambdaDef:
        let
            name = n.children[0]
            exp = n.children[1]
            scope = newScope(scope)
        assert name.kind == akId
        Term.Lambda(@[newIdentDef(newTerm(name, scope))], newBody(newTerm(exp, scope), scope))
    of akInfix:
        assert n.children.len == 3
        let
            callee = n.children[0]
            args = n.children[1..2]
        Term.Apply(newTerm(callee, scope), args.mapIt(newTerm(it, scope)))
    of akPrefix:
        assert n.children.len == 2
        let
            callee = n.children[0]
            args = n.children[1..1]
        Term.Apply(newTerm(callee, scope), args.mapIt(newTerm(it, scope)))
    of akDot:
        let
            callee = n.children[1]
            args = n.children[0..0]
        if callee.kind == akId:
            case callee.strVal
            of "typeof":
                assert args.len == 1
                return Term.TypeOf(newTerm(args[0], scope))
        Term.Apply(newTerm(callee, scope), args.mapIt(newTerm(it, scope)))
    of akCommand, akCall:
        let
            callee = n.children[0]
            args = n.children[1..^1]
        if callee.kind == akId:
            case callee.strVal
            of "typeof":
                assert args.len == 1
                return Term.TypeOf(newTerm(args[0], scope))
        if callee.kind == akDot:
            newTerm(akCall.newTreeNode(@[callee.children[1], callee.children[0]] & args), scope)
        else:
            Term.Apply(newTerm(callee, scope), args.mapIt(newTerm(it, scope)))
    of akInt:
        Term.Integer(n.intVal)
    of akFloat:
        Term.Float(n.floatVal)
    of akString:
        Term.String(n.strVal)
    of akBool:
        Term.Bool(n.boolval)
    of akId:
        Term.Id(n.strVal)
    else:
        echo n.kind
        echo n
        assert false, "notimplemented"
        nil
    result.loc = n.loc
