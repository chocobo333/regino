
import sequtils

import ../ast
import il

    
proc newTerm*(n: AstNode): Term =
    result = case n.kind
    of akEmpty:
        Term.Unit
    of akStmtList:
        Term.Seq(n.children.map(newTerm))
    of akLetSection:
        let ts = n.children.mapIt(
            block:
                assert it.kind == akIdentDef
                assert it.children.len == 3
                let
                    id = it.children[0]
                    typ = it.children[1]
                    default = it.children[2]
                assert id.kind == akId, ""
                assert typ.isEmpty(), "notimplemented type annotation"
                assert not default.isNil, "let section needs initialization"
                Term.Let(id.strVal, newTerm(default))
        )
        Term.Seq(ts)
    of akAliasSection:
        let ts = n.children.mapIt(
            block:
                assert it.kind == akIdentDef
                assert it.children.len == 3
                let
                    id = it.children[0]
                    typ = it.children[1]
                    default = it.children[2]
                assert id.kind == akId, ""
                assert typ.isEmpty(), "notimplemented type annotation"
                assert not default.isNil, "alias section needs initialization"
                Term.TypeDef(id.strVal, newTerm(default))
        )
        Term.Seq(ts)
    of akFuncDef:
        let
            fname = n.children[0]
            funty = n.children[1]
            rety = funty.children[0]
            paramty = funty.children[1..^1]
            metadata = n.children[2]
            body = n.children[3]
        assert fname.kind == akId
        assert rety.kind == akId
        for e in paramty:
            assert e.children[0].kind == akId
            assert e.children[2].isEmpty, "default value is not supported"
        let
            meta = if metadata.isEmpty: nil else: newTerm(metadata).metadata
            params = paramty.mapIt(ParamDef(name: Identifier(name: it.children[0].strVal), typ: newTerm(it.children[1])))
        if not meta.isNil and meta.kind == MetadataKind.ImportLL:
            assert body.isEmpty
        else:
            assert not body.isEmpty
        let fn = newFunction(fname.strVal, params, newTerm(rety), newTerm(body), meta)
        Term.FuncDef(fn)
    of akMetadata:
        let
            name = n.children[0]
            param = if n.children.len == 1: nil else: newTerm(n.children[1])
        assert name.kind == akId
        let metadata = case name.strVal
        of "link":
            Metadata.Link(param)
        of "importll":
            Metadata.ImportLL(param)
        else:
            Metadata.UserDef(name.strVal, param)
        Term.Metadat(metadata)
    of akLambdaDef:
        let
            name = n.children[0]
            exp = n.children[1]
        assert name.kind == akId
        Term.Lam(name.strVal, newTerm(exp))
    of akCall:
        let
            callee = n.children[0]
            args = n.children[1..^1]
        if callee.kind == akId:
            case callee.strVal
            of "typeof":
                assert args.len == 1
                return Term.TypeOf(newTerm(args[0]))
        Term.App(newTerm(callee), args.mapIt(newTerm(it)))
    of akInt:
        Term.Int(n.intVal)
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
    result.lineInfo = n.lineInfo