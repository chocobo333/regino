
import sequtils
import tables
import options

import il

import llvm except Value, ValueKind, Type, Module


type
    LType* = llvm.Type
    LValue* = llvm.Value
    LModule* = llvm.Module
    Module* = ref object
        module*: llvm.Module
        cxt*: Context
        curFun*: LValue
        curBuilder*: Builder
        curBB*: BasicBlock
        type2llvmType*: Table[ref Value, LType]
        linkModules*: seq[LModule]
        linkFuncs*: seq[LValue]
        malloc: FunctionValue
        targetdata: TargetData

proc newModule*(name: string = "main"): Module =
    let
        cxt = newContext()
        module = llvm.newModule(name, cxt)
        pty = pointerType(cxt.intType(8))
        fnty = functionType(pty, @[cast[LType](cxt.intType(32))])
        fn = module.addFunction("malloc", fnty)
    result = Module(
        module: module,
        cxt: cxt,
        curBuilder: newBuilder(cxt),
        type2llvmType: initTable[ref Value, LType](),
        # scopes: newScope()
        malloc: fn,
        targetdata: module.targetData
    )

var strtyp: LType
proc newLType(typ: Value, module: Module): LType =
    let
        cxt = module.cxt
    case typ.kind
    of il.ValueKind.Unit:
        cxt.voidType()
    of il.ValueKind.Bool:
        cxt.intType(1)
    of il.ValueKind.Integer:
        cxt.intType(typ.bits)
    of il.ValueKind.Float:
        cxt.floatType()
    of il.ValueKind.String:
        once:
            strtyp = cxt.createStruct("string")
            strtyp.body = @[pointerType(cxt.intType(8)), cxt.intType(32), cxt.intType(32)]
        strtyp
    of il.ValueKind.Ptr:
        pointerType(newLType(typ.pointee, module))
    of il.ValueKind.Pair:
        cxt.createStruct(@[typ.first.newLType(module), typ.second.newLType(module)])
    of il.ValueKind.Record:
        cxt.createStruct(toSeq(typ.members.values).mapIt(it.newLType(module)))
    # of il.ValueKind.Arrow:
    #     let
    #         paramty = typ.paramty.mapIt(it.newLType(module))
    #         rety = typ.rety.newLType(module)
    #     pointerType(functionType(rety, paramty))
    # of il.ValueKind.Sigma:
    #     cxt.createStruct(@[typ.first.newLType(module), typ.second.newLType(module)])
    of il.ValueKind.Pi:
        let
            paramty = typ.params.mapIt(it.newLType(module))
            rety = typ.rety.newLType(module)
        pointerType(functionType(rety, paramty))
    # of il.ValueKind.Pair:
    #     cxt.createStruct(@[typ.first.newLType(module), typ.second.newLType(module)])
    # of il.ValueKind.Arrow:
    #     let
    #         paramty = typ.paramty.mapIt(it.newLType(module))
    #         rety = typ.rety.newLType(module)
    #     pointerType(functionType(rety, paramty))
    of il.ValueKind.Link:
        newLType(typ.to, module)
    else:
        echo typ
        echo typ.kind
        assert false, "notimplemnted"
        nil

converter toIntegerType(self: LType): IntegerType =
    cast[IntegerType](self)
converter toFunctionType(self: LType): FunctionType =
    cast[FunctionType](self)
converter toFunctionValue(self: LValue): FunctionValue =
    cast[FunctionValue](self)

proc sym(self: Expression|Statement): Impl =
    if self.typ in self.typ.symbol.get.instances:
        result = self.typ.symbol.get.instances[self.typ]
    else:
        result = Impl()
        self.typ.symbol.get.instances[self.typ] = result
proc `sym=`*(self: Expression|Statement, sym: Impl) =
    self.typ.symbol.get.instances[self.typ] = sym


proc codegen(self: Expression, module: Module, global: bool = false) =
    case self.kind
    of ExpressionKind.Literal:
        discard
    of ExpressionKind.Ident:
        discard
    of ExpressionKind.Tuple:
        discard
    of ExpressionKind.Array:
        discard
    of ExpressionKind.Record:
        discard
    of ExpressionKind.If:
        discard
    of ExpressionKind.When:
        discard
    of ExpressionKind.Case:
        discard
    of ExpressionKind.Call:
        discard
    of ExpressionKind.Command:
        discard
    of ExpressionKind.Dot:
        discard
    of ExpressionKind.Bracket:
        discard
    of ExpressionKind.Binary:
        discard
    of ExpressionKind.Prefix:
        discard
    of ExpressionKind.Postfix:
        discard
    of ExpressionKind.Block:
        discard
    of ExpressionKind.Lambda:
        discard
    of ExpressionKind.Malloc:
        discard
    of ExpressionKind.Typeof:
        discard
    of ExpressionKind.Ref:
        discard
    of ExpressionKind.FnType:
        discard
    of ExpressionKind.Fail:
        discard
proc codegen(self: Statement, module: Module, global: bool = false) =
    case self.kind
    of StatementKind.For:
        discard
    of StatementKind.While:
        discard
    of StatementKind.Loop:
        discard
    of StatementKind.LetSection:
        discard
    of StatementKind.VarSection:
        discard
    of StatementKind.ConstSection:
        discard
    of StatementKind.TypeSection:
        discard
    of StatementKind.Asign:
        discard
    of StatementKind.Funcdef:
        discard
    of StatementKind.Meta:
        discard
    of StatementKind.Discard:
        discard
    of StatementKind.Comments:
        discard
    of StatementKind.Expression:
        discard
    of StatementKind.Fail:
        discard
proc codegen*(self: Program, module: Module, global: bool = false) =
    let
        rety = self.typ.newLType(module)
    echo rety
    for s in self.stmts:
        s.codegen(module, global)
