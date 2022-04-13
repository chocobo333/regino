
import sequtils
import tables
import options

import il
import utils

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
        if typ.bits == 0:
            cxt.intType(wordSize())
        else:
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

proc sym(self: Expression|Statement|Ident): Impl =
    if self.typ in self.typ.symbol.get.instances:
        result = self.typ.symbol.get.instances[self.typ]
    else:
        result = Impl()
        self.typ.symbol.get.instances[self.typ] = result
proc `sym=`*(self: Expression|Statement, sym: Impl) =
    self.typ.symbol.get.instances[self.typ] = sym


proc codegen(self: Literal, module: Module, global: bool = false): LValue =
    case self.kind
    of LiteralKind.unit:
        nil
    of LiteralKind.bool:
        let
            b = self.boolval
            boolty = newLType(Value.Bool, module)
        boolty.constInt(if b: 1 else: 0)
    of LiteralKind.integer:
        let
            i = self.intval
            bits = self.intbits
            intty = newLType(Value.Integer(bits), module)
        intty.constInt(int i)
    of LiteralKind.float:
        nil
    of LiteralKind.char:
        nil
    of LiteralKind.string:
        nil
    of LiteralKind.Univ:
        nil
proc codegen(self: Ident, module: Module, global: bool = false, lval: bool = false, ): LValue =
    let
        name = self.name
        sym = self.sym
        val = sym.val
        ty = sym.lty
    if val.kind == llvm.ValueKind.FunctionValueKind:
        val
    else:
        if lval:
            val
        else:
            module.curBuilder.load(ty, val, name)
proc codegen(self: Expression, module: Module, global: bool = false, lval: bool = false): LValue =
    case self.kind
    of ExpressionKind.Literal:
        self.litval.codegen(module, global)
    of ExpressionKind.Ident:
        echo self.ident
        let v = self.ident.codegen(module, global, lval)
        echo v
        v
    of ExpressionKind.Tuple:
        nil
    of ExpressionKind.Array:
        nil
    of ExpressionKind.Record:
        nil
    of ExpressionKind.If:
        nil
    of ExpressionKind.When:
        nil
    of ExpressionKind.Case:
        nil
    of ExpressionKind.Call:
        nil
    of ExpressionKind.Command:
        nil
    of ExpressionKind.Dot:
        nil
    of ExpressionKind.Bracket:
        nil
    of ExpressionKind.Binary:
        nil
    of ExpressionKind.Prefix:
        nil
    of ExpressionKind.Postfix:
        nil
    of ExpressionKind.Block:
        nil
    of ExpressionKind.Lambda:
        nil
    of ExpressionKind.Malloc:
        nil
    of ExpressionKind.Typeof:
        nil
    of ExpressionKind.Ref:
        nil
    of ExpressionKind.FnType:
        nil
    of ExpressionKind.Fail:
        nil
proc codegen(self: Pattern, module: Module, typ: Value, val: LValue, global: bool = false) =
        case self.kind
        of PatternKind.Literal:
            discard
        of PatternKind.Ident:
            let
                id = self.ident
                sym = id.sym
                name = id.name
                typ = newLType(typ, module)
            echo module.curBuilder.repr
            let
                p = if global: module.module.newGlobal(typ, name) else: module.curBuilder.alloca(typ, name)
            sym.val = p
            sym.lty = typ
            discard module.curBuilder.store(val, p, $self)
        of PatternKind.Tuple:
            assert false, "notimplemented"
            # assert typ.kind == ValueKind.Pair
            # case self.patterns.len
            # of 0:
            #     discard
            # of 1:
            #     codegen(self.patterns[0], module, typ.first, module.curBuilder.extractvalue(val, 0, $self), global)
            # of 2:
            #     codegen(self.patterns[0], module, typ.first, module.curBuilder.extractvalue(val, 0, $self), global)
            #     codegen(self.patterns[1], module, typ.second, module.curBuilder.extractvalue(val, 1, $self), global)
            # else:
            #     codegen(self.patterns[0], module, typ.first, module.curBuilder.extractvalue(val, 0, $self), global)
            #     codegen(Expression.Tuple(self.patterns[1..^1]), module, typ.second, module.curBuilder.extractvalue(val, 1, $self), global)
        of PatternKind.Record:
            assert false, "notimplemented"
            for (i, idpat) in self.members.pairs:
                let (id, pat) = idpat
                codegen(pat, module, typ.members[id], module.curBuilder.extractvalue(val, i, $self & "." & $id), global)
        of PatternKind.UnderScore:
            discard
        else:
            assert false, "notimplemented"
proc codegen(self: Statement, module: Module, global: bool = false): LValue =
    case self.kind
    of StatementKind.For:
        nil
    of StatementKind.While:
        nil
    of StatementKind.Loop:
        nil
    of StatementKind.LetSection, StatementKind.VarSection:
        for e in self.iddefs:
            let
                pat = e.pat
                default = e.default.get
            codegen(pat, module, default.typ, default.codegen(module, global))
        nil
    of StatementKind.ConstSection:
        nil
    of StatementKind.TypeSection:
        nil
    of StatementKind.Asign:
        nil
    of StatementKind.Funcdef:
        nil
    of StatementKind.Meta:
        nil
    of StatementKind.Discard:
        nil
    of StatementKind.Comments:
        nil
    of StatementKind.Expression:
        self.expression.codegen(module, global, lval=false)
    of StatementKind.Fail:
        nil

template enter(self: Module, fn: FunctionValue, body: untyped) =
    block:
        let
            tmpBB = self.curBB
            tmpFn = self.curFun
            bb = fn.appendBasicBlock("entry")
        module.curFun = fn
        module.curBuilder.atEndOf(bb)
        module.curBB = bb
        body
        module.curBB = tmpBB
        module.curFun = tmpFn
        if not tmpBB.isNil:
            module.curBuilder.atEndOf(tmpBB)
proc codegen*(self: Program, module: Module, global: bool = false) =
    let
        rety = self.typ.newLType(module)
        fnty = functionType(rety, @[])
        fn = module.module.addFunction("main", fnty)
    module.enter fn:
        var res: LValue
        for s in self.stmts:
            res = s.codegen(module, global)
        if rety != module.cxt.voidType:
            module.curBuilder.ret(res)
        else:
            module.curBuilder.retVoid
    debug fn
