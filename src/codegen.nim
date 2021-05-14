
import tables

import sema/types
import sema/ir

import llvm


type
    Module* = ref object
        module*: llvm.Module
        cxt*: Context
        curFun*: Value
        curBuilder*: Builder
        type2llvmType*: Table[types.Type, llvm.Type]
        linkModules*: seq[llvm.Module]
        linkFuncs*: seq[llvm.Value]

proc newModule*(name: string = "main"): Module =
    let
        cxt = newContext()
    Module(
        module: llvm.newModule(name, cxt),
        cxt: cxt,
        curBuilder: newBuilder(cxt),
        type2llvmType: initTable[types.Type, llvm.Type]()
    )

proc convertType*(self: Module, typ: types.Type): llvm.Type =
    case typ.kind
    of tkNone:
        assert false
        nil
    of tkUnit:
        assert false
        nil
    of tkInt:
        intType(self.cxt, 32)
    of tkFloat:
        floatType(self.cxt)
    else:
        assert false
        nil
proc codegen(self: Expr, module: Module): Value =
    case self.kind
    of ekCall:
        discard
    of ekBinOp:
        discard
    of ekIfExpr:
        discard
    of ekLoopExpr:
        discard
    of ekBlockExpr:
        discard
    of ekChar:
        discard
    of ekInt:
        let typ = intType(module.cxt, if self.intsize == 0: 32 else: self.intsize)
        return typ.constInt(int self.intval, false)
    of ekFloat:
        let typ = floatType(module.cxt)
        return typ.constReal(cdouble self.floatval)
    of ekString:
        discard
    of ekBool:
        discard
    of ekId:
        discard
proc vardeclare(self: IdentDef, module: Module) =
    let
        pat = self.id
    case pat.kind
    of pkId:
        let typ = pat.id.sym.typ
        assert not typ.isNil
        let
            ltyp = module.convertType(typ)
            a = module.curBuilder.alloca(ltyp, $pat)
        if not default.isNil:
            discard module.curBuilder.store(codegen(self.default, module), a)
    else:
        discard
proc codegen(self: Statement, module: Module) =
    case self.kind
    of stkComment:
        discard
    of stkVarDecl:
        for e in self.iddefs:
            vardeclare e, module
    of stkLetDecl:
        discard
    of stkConstDecl:
        discard
    of stkAliasDecl:
        discard
    of stkFuncDef:
        discard
    of stkTempDef:
        discard
    of stkMacroDef:
        discard
    of stkIterDef:
        discard
    of stkExprStmt:
        discard
    of stkAsign:
        discard
    of stkMetadata:
        discard
proc codegen*(self: Program): string =
    var
        cxt = newContext()
        module = newModule()
        i32 = intType(module.cxt, 32)
        mainty = functionType(i32, @[])
        main = module.module.addFunction("main", mainty)
        bb = main.appendBasicBlock("entry", cxt)
    module.curFun = main
    module.curBuilder.atEndOf(bb)
    for e in self.stmts:
        e.codegen(module)
    echo module.module