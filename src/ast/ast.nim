
import ../lineInfos


type
    AstKind = enum
        akFailed
        # akDefinition
        akFuncDef
        akTempDef
        akMacroDef
        akIterDef
        akAsign
        akBlockExpr
        akIfExpr
        akElifBlock
        akElseBlock
        akCall
        akChar
        akInt
        akFloat
        akString
        akId
    # DefKind = enum
    #     Func
    #     Template
    #     Macro
    #     Iterator

const
    akLiteral = {akInt..akString}

type
    AstNode = object
        lineInfo: LineInfo
        case kind: AstKind
        of akChar..akInt:
            intVal: BiggestInt
        of akFloat:
            floatVal: BiggestFloat
        of akString..akId:
            strVal: string
        else:
            children: seq[AstNode]
