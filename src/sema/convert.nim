
import algorithm
import sequtils

import ../ast
import il

    
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
    of akInt:
        Term.Int(n.intVal)
    of akBool:
        Term.Bool(n.boolval)
    of akId:
        Term.Id(n.strVal)
    else:
        echo n.kind
        assert false, "notimplemented"
        nil