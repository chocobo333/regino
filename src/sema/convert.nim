
import algorithm
import sequtils

import ../ast
import il


proc newMetadata*(n: AstNode): Metadata =
    assert n.kind == akMetadata
    echo n
    
proc newTerm*(n: AstNode): Term =
    case n.kind
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
                Term.Let(id.strVal, newTerm(default))
        )
        Term.Seq(ts)
    of akFuncDef:
        let
            fname = n.children[0]
            funty = n.children[1]
            rety = funty.children[0]
            paramty = funty.children[1]
            metadata = n.children[2]
            body = n.children[3]
        assert fname.kind == akId
        assert rety.kind == akId
        assert funty.children.len == 2
        assert paramty.children[2].isEmpty
        assert metadata.isEmpty
        assert not body.isEmpty
        Term.FuncDef(fname.strVal, paramty.children[0].strVal, newTerm(paramty.children[1]), newTerm(rety), newTerm(body))
    of akCall:
        let
            callee = n.children[0]
            args = n.children[1..^1]
        assert args.len == 1
        if callee.kind == akId:
            case callee.strVal
            of "typeof":
                assert args.len == 1
                return Term.TypeOf(newTerm(args[0]))
        Term.App(newTerm(callee), newTerm(args[0]))
    of akInt:
        Term.Int(n.intVal)
    of akBool:
        Term.Bool(n.boolval)
    of akId:
        Term.Id(n.strVal)
    else:
        echo n.kind
        echo n
        assert false, "notimplemented"
        nil