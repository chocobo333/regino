
import sequtils
import tables

import sema/[
    il,
    types
]

import llvm except Type, Module


type
    LType* = llvm.Type
    LModule* = llvm.Module
    Module* = ref object
        module*: llvm.Module
        cxt*: Context
        curFun*: Value
        curBuilder*: Builder
        curBB*: BasicBlock
        type2llvmType*: Table[Type, LType]
        linkModules*: seq[LModule]
        linkFuncs*: seq[Value]
#         scopes*: Scope
#     Scope* = ref object
#         parent*: Scope
#         vars*: Table[string, (LType, Value)]

# proc newScope(parent: Scope = nil): Scope =
#     Scope(
#         parent: parent,
#         vars: initTable[string, (LType, Value)]()
#     )
proc newModule*(name: string = "main"): Module =
    let
        cxt = newContext()
    Module(
        module: llvm.newModule(name, cxt),
        cxt: cxt,
        curBuilder: newBuilder(cxt),
        type2llvmType: initTable[Type, LType](),
        # scopes: newScope()
    )
# proc pushScope(self: Module) =
#     self.scopes = newScope(self.scopes)
# proc popScope(self: Module) =
#     self.scopes = self.scopes.parent
# proc extend(self: Scope, name: string, typ: LType, val: Value) =
#     self.vars[name] = (typ, val)
# proc lookup(self: Scope, name: string): (LType, Value) =
#     var scope = self
#     while not scope.isNil:
#         if name in scope.vars:
#             return scope.vars[name]
#         scope = scope.parent
#     assert false, fmt"{name} was not declared"

proc newLType(typ: Type, module: Module): LType =
    let
        cxt = module.cxt
    case typ.kind
    of types.TypeKind.Unit:
        nil
    of types.TypeKind.Bool:
        cxt.intType(1)
    of types.TypeKind.Int:
        cxt.intType(32)
    of types.TypeKind.String:
        assert false, "notimplemnted"
        nil
    else:
        echo typ
        assert false, "notimplemnted"
        nil

converter toIntegerType(self: LType): IntegerType =
    cast[IntegerType](self)
converter toFunctionType(self: LType): FunctionType =
    cast[FunctionType](self)
converter toFunctionValue(self: Value): FunctionValue =
    cast[FunctionValue](self)


proc builtin*(self: Module) =
    discard
proc codegen*(self: Term, module: Module, global: bool = false, lval: bool = false): Value =
    case self.kind
    of TermKind.Unit:
        nil
    of TermKind.Bool:
        let
            b = self.b
            boolty = newLType(Type.Bool, module)
        boolty.constInt(if b: 1 else: 0)
    of TermKind.Int:
        let
            i = self.i
            intty = newLType(Type.Int, module)
        intty.constInt(int i)
    of TermKind.Id:
        let
            name = self.id.name
            sym = self.id.symbol
            val = sym.val
            ty = sym.lty
        if lval:
            val
        else:
            module.curBuilder.load(ty, val, name)
    of TermKind.Let:
        let
            id = self.name
            name = id.name
            sym = id.symbol
            default = self.default
            typ = newLType(default.typ, module)
            p = module.curBuilder.alloca(typ, name)
        sym.val = p
        sym.lty = typ
        discard module.curBuilder.store(default.codegen(module, global), p, $self)
        nil
    of TermKind.TypeDef:
        nil
    of TermKind.FuncDef:
        let
            fn = self.fn
            id = fn.id
            name = id.name
            sym = id.symbol
            paramty = fn.params.mapIt(newLType(it.typ.typ.typ, module))
            rety = fn.rety.typ.typ
        var
            cxt = module.cxt
            rety2 = if rety.isNil: nil else: newLType(rety, module)
            fnty = functionType(rety2, paramty)
        var
            fn2 = module.module.addFunction(name, fnty)
        sym.val = fn2
        sym.lty = fnty
        if not fn.metadata.isNil and fn.metadata.kind == MetadataKind.ImportLL:
            return nil
        var
            tmpBB = module.curBB
            tmpFun = module.curFun
            bb = fn2.appendBasicBlock("entry", cxt)
        module.curFun = fn2
        module.curBuilder.atEndOf(bb)
        module.curBB = bb
        for i in 0..<fn.params.len:
            let
                name = fn.params[i].name.name
                typ = paramty[i]
                p = module.curBuilder.alloca(typ, name)
            fn.params[i].name.symbol.val = p
            fn.params[i].name.symbol.lty = typ
            discard module.curBuilder.store(fn2.param(i), p, name)
        var ret = fn.body.codegen(module)
        if not ret.isNil:
            discard module.curBuilder.ret(ret)
        module.curBB = tmpBB
        module.curFun = tmpFun
        if not tmpBB.isNil:
            module.curBuilder.atEndOf(tmpBB)
        nil
    of TermKind.App:
        let
            callee = self.callee
            args = self.args
            callee2 = codegen(callee, module, lval = true)
            args2 = args.mapIt(codegen(it, module))
            rety = newLType(self.typ, module)
        module.curBuilder.call(callee2, args2, $self)
    of TermKind.If:
        let cond = self.cond.codegen(module)
        var
            thenb = module.curFun.appendBasicBlock("then")
            elseb = module.curFun.appendBasicBlock("else")
            ifcont = module.curFun.appendBasicBlock("ifcont")
        discard module.curBuilder.condBr(cond, thenb, elseb)

        module.curBuilder.atEndOf(thenb)
        let thenv = self.thent.codegen(module)
        module.curBuilder.br(ifcont)

        module.curBuilder.atEndOf(elseb)
        let elsev = self.elset.codegen(module)
        module.curBuilder.br(ifcont)

        module.curBuilder.atEndOf(ifcont)
        module.curBB = ifcont
        if self.typ.kind == types.TypeKind.Unit:
            nil
        else:
            let phi = module.curBuilder.phi(newLType(self.typ, module), "")
            phi.addIncoming(@[(thenv, thenb), (elsev, elseb)])
            phi
    of TermKind.Seq:
        var ret: Value = nil
        for e in self.ts:
            ret = e.codegen(module, global)
        ret
    of TermKind.Metadata:
        nil
    else:
        echo self
        assert false, "notimplemented"
        nil