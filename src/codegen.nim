
import sequtils
import tables
import options
import uri
import algorithm
from os import `/`, splitPath, absolutePath

import il
import sema/region/infer
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
        cxt.createStruct(toSeq(typ.members.pairs).sortedByIt(it[0].name).mapIt(it[1].newLType(module)))
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
        if typ.rety.hasRegion:
            functionType(cxt.voidType, paramty & pointerType(rety))
        else:
            functionType(rety, paramty)
    # of il.ValueKind.Pair:
    #     cxt.createStruct(@[typ.first.newLType(module), typ.second.newLType(module)])
    # of il.ValueKind.Arrow:
    #     let
    #         paramty = typ.paramty.mapIt(it.newLType(module))
    #         rety = typ.rety.newLType(module)
    #     pointerType(functionType(rety, paramty))
    of il.ValueKind.Distinct:
        newLType(typ.base, module)
    of il.ValueKind.Link:
        newLType(typ.to, module)
    else:
        debug typ
        debug typ.kind
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
        let
            f = self.floatval
            bits = self.floatbits
            floatty = newLType(Value.Float(bits), module)
        floatty.constReal(f)
    of LiteralKind.char:
        nil
    of LiteralKind.string:
        let
            s = self.strval
        if strtyp.isNil:
            discard newLType(Value.String, module)
        let
            conststr = module.cxt.constString(s)
            global = module.module.newGlobal(conststr.typ, s)
            inty = newLType(Value.Integer(32), module)
        global.initializer = conststr
        global.constant = true
        constStruct(
            strtyp,
            @[
                constGep(
                    global,
                    @[inty.constInt(0), inty.constInt(0)]
                ),
                newLType(Value.Integer(32), module).constInt(s.len),
                newLType(Value.Integer(32), module).constInt(s.len)
            ]
        )
    of LiteralKind.Univ:
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
proc codegen(self: Ident, module: Module, global: bool = false, lval: bool = false, ): LValue =
    let
        name = self.name
        sym = self.sym
    if self.typ.symbol.get.kind == SymbolKind.Field:
        if sym.val.isNil and sym.lty.isNil:
            let
                symbol = self.typ.symbol.get
                index = self.typ.symbol.get.index
                obj = symbol.typ.params[0].newLType(module)
                typ = symbol.typ.rety.newLType(module)
                fnty = symbol.typ.newLType(module)
                fn = module.module.addFunction($self, fnty)
            sym.val = fn
            sym.lty = fnty
            module.enter fn:
                let
                    arg = fn.param(0)
                    indices = [Literal.integer(0, 32), Literal.integer(index, 32)]
                    ret = module.curBuilder.extractvalue(arg, index, "")
                if symbol.typ.rety.hasRegion:
                    discard module.curBuilder.store(ret, fn.param(1))
                    discard module.curBuilder.retVoid()
                else:
                    discard module.curBuilder.ret(ret)
    if sym.val.kind == llvm.ValueKind.FunctionValueKind:
        sym.val
    else:
        if lval:
            sym.val
        else:
            module.curBuilder.load(sym.lty, sym.val, name)
proc codegen(self: Statement, module: Module, global: bool = false): LValue
proc codegen(self: Suite, module: Module, global: bool = false): LValue =
    for e in self.stmts:
        result = e.codegen(module, global)
proc codegen(self: Expression, module: Module, global: bool = false, lval: bool = false): LValue =
    case self.kind
    of ExpressionKind.Literal:
        self.litval.codegen(module, global)
    of ExpressionKind.Ident:
        self.ident.codegen(module, global, lval)
    of ExpressionKind.Tuple:
        proc makeTuple(typ: Value, terms: seq[Expression]): LValue =
            if typ.second.kind == il.ValueKind.Pair:
                # if typ.second.kind == il.ValueKind.Sigma:
                var ret = module.curBuilder.insertvalue(typ.newLType(module).undef, makeTuple(typ.second, terms[1..^1]), 1, $terms[1..^1])
                module.curBuilder.insertvalue(ret, terms[0].codegen(module, global), 0, $terms[0])
            else:
                assert terms.len == 2, ""
                var ret = module.curBuilder.insertvalue(typ.newLType(module).undef, terms[1].codegen(module, global), 1, $terms[1])
                module.curBuilder.insertvalue(ret, terms[0].codegen(module, global), 0, $terms[0])
        makeTuple(self.typ, self.exprs)
    of ExpressionKind.Array:
        nil
    of ExpressionKind.Record:
        let
            typ = self.typ.newLType(module)
            alloca = module.curBuilder.alloca(typ)
        var i = 0
        for (id, val) in self.members.sortedByIt(it[0].name):
            let
                indices = [Literal.integer(0, 32).codegen(module), Literal.integer(i, 32).codegen(module)]
                p = module.curBuilder.gep2(typ, alloca, indices, "")
            discard module.curBuilder.store(val.codegen(module, global), p)
            inc i
        module.curBuilder.load(typ, alloca)
    of ExpressionKind.If:
        let
            thenbs = self.elifs.mapIt(module.curFun.appendBasicBlock("then"))
            elseb = module.curFun.appendBasicBlock("else")
            ifcont = module.curFun.appendBasicBlock("ifcont")
        var
            thenvs: seq[LValue] = @[]
        for i in 0..<thenbs.len:
            if i == thenbs.len-1:
                discard module.curBuilder.condBr(self.elifs[i][0].codegen(module), thenbs[i], elseb)
            else:
                var elifb = module.curFun.appendBasicBlock("elif")
                discard module.curBuilder.condBr(self.elifs[i][0].codegen(module), thenbs[i], elifb)
                module.curBuilder.atEndOf(elifb)
                module.curBB = elifb
        for i in 0..<thenbs.len:
            module.curBuilder.atEndOf(thenbs[i])
            module.curBB = thenbs[i]
            let thenv = self.elifs[i][1].codegen(module)
            thenvs.add thenv
            module.curBuilder.br(ifcont)

        module.curBuilder.atEndOf(elseb)
        module.curBB = elseb
        let elsev = self.elseb.get.codegen(module)
        module.curBuilder.br(ifcont)

        module.curBuilder.atEndOf(ifcont)
        module.curBB = ifcont
        if self.typ == Value.Unit:
            nil
        else:
            let phi = module.curBuilder.phi(newLType(self.typ, module), "")
            phi.addIncoming(thenvs.zip(thenbs) & @[(elsev, elseb)])
            phi
    of ExpressionKind.When:
        nil
    of ExpressionKind.Case:
        nil
    of ExpressionKind.Call, ExpressionKind.Command:
        let
            callee = self.callee
            args = self.args
            callee2 = codegen(callee, module)
            args2 = args.mapIt(codegen(it, module))
            rety = newLType(self.typ, module)
        # TODO: check returning void
        # module.curBuilder.call(callee2, args2, $self)
        # if self.typ.hasRegion:
        #     let ret = module.curBuilder.alloca(rety, "*" & $self)
        #     discard module.curBuilder.call(callee2, args2 & ret)
        #     module.curBuilder.load(rety, ret, $self)
        # else:
        let res = if self.typ.hasRegion:
            let ret = module.curBuilder.alloca(rety, "*" & $self)
            discard module.curBuilder.call(callee2, args2 & ret)
            module.curBuilder.load(rety, ret, $self)
        else:
            module.curBuilder.call(callee2, args2)
        res
    of ExpressionKind.Dot:
        let
            callee = self.rhs
            args = @[self.lhs]
            callee2 = codegen(callee, module)
            args2 = args.mapIt(codegen(it, module))
            rety = newLType(self.typ, module)
        # TODO: check returning void
        # module.curBuilder.call(callee2, args2, $self)
        # if self.typ.hasRegion:
        #     let ret = module.curBuilder.alloca(rety, "*" & $self)
        #     discard module.curBuilder.call(callee2, args2 & ret)
        #     module.curBuilder.load(rety, ret, $self)
        # else:
        let res = if self.typ.hasRegion:
            let ret = module.curBuilder.alloca(rety, "*" & $self)
            discard module.curBuilder.call(callee2, args2 & ret)
            module.curBuilder.load(rety, ret, $self)
        else:
            module.curBuilder.call(callee2, args2)
        res
    of ExpressionKind.Bracket:
        nil
    of ExpressionKind.Binary:
        let
            op = self.op
            lhs = self.lhs
            rhs = self.rhs
            op2 = op.codegen(module)
            lhs2 = lhs.codegen(module)
            rhs2 = rhs.codegen(module)
            rety = newLType(self.typ, module)
        # TODO: check returning void
        # module.curBuilder.call(callee2, args2, $self)
        if self.typ.hasRegion:
            let ret = module.curBuilder.alloca(rety, "*" & $self)
            discard module.curBuilder.call(op2, @[lhs2, rhs2] & ret)
            module.curBuilder.load(rety, ret, $self)
        else:
            module.curBuilder.call(op2, @[lhs2, rhs2])
    of ExpressionKind.Prefix:
        let
            callee = self.op
            args = self.expression
            callee2 = codegen(callee, module)
            args2 = @[args.codegen(module)]
            rety = newLType(self.typ, module)
        # TODO: check returning void
        # module.curBuilder.call(callee2, args2, $self)
        # if self.typ.hasRegion:
        #     let ret = module.curBuilder.alloca(rety, "*" & $self)
        #     discard module.curBuilder.call(callee2, args2 & ret)
        #     module.curBuilder.load(rety, ret, $self)
        # else:
        let res = if self.typ.hasRegion:
            let ret = module.curBuilder.alloca(rety, "*" & $self)
            discard module.curBuilder.call(callee2, args2 & ret)
            module.curBuilder.load(rety, ret, $self)
        else:
            module.curBuilder.call(callee2, args2)
        res
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
    of ExpressionKind.IntCast:
        raise
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
proc link(self: Metadata, module: Module) =
    let param = self.params[0]
    assert param.kind == ExpressionKind.Literal and param.litval.kind == LiteralKind.string
    let
        path = param.loc.uri.path.splitPath.head.absolutePath / param.litval.strval
        f = open(path)
        s = f.readAll()
        module2 = llvm.parseIr(module.cxt, path)
    if module2.isSome:
        let m = module2.get
        module.linkModules.add m
        for fn in m.funcs:
            module.linkFuncs.add fn
    else:
        # TODO: err msg
        assert false
    f.close()
proc setParam(self: Pattern, module: Module, typ: Value, val: LValue) =
    case self.kind
    of PatternKind.Literal:
        discard
    of PatternKind.Ident:
        let
            id = self.ident
            name = id.name
            sym = id.sym
            typ = newLType(typ, module)
            p = module.curBuilder.alloca(typ, name)
        sym.val = p
        sym.lty = typ
        discard module.curBuilder.store(val, p, name)
    of PatternKind.Tuple:
        case self.patterns.len
        of 0:
            discard
        of 1:
            setParam(self.patterns[0], module, typ.first, module.curBuilder.extractvalue(val, 0, $self))
        of 2:
            setParam(self.patterns[0], module, typ.first, module.curBuilder.extractvalue(val, 0, $self))
            setParam(self.patterns[1], module, typ.second, module.curBuilder.extractvalue(val, 1, $self))
        else:
            setParam(self.patterns[0], module, typ.first, module.curBuilder.extractvalue(val, 0, $self))
            setParam(Pattern.Tuple(self.patterns[1..^1]), module, typ.second, module.curBuilder.extractvalue(val, 1, $self))
    of PatternKind.Record:
        # TODO:
        discard
    of PatternKind.UnderScore:
        discard
proc codegen(self: Function, module: Module, global: bool = false) =
    if self.param.implicit.len > 0:
        #     # TODO: for quantified function
        #     for val in self.fn.id.typ.symbol.get.instances.keys:
        #         assert self.fn.id.typ.symbol.get.instances[val].instance.isSome
        #         self.fn.id.typ.symbol.get.instances[val].val = self.fn.id.typ.symbol.get.instances[val].instance.get.codegen(module)
        #         self.fn.id.typ.symbol.get.instances[val].lty = newLType(val, module)
        return
    let
        fn = self
        id = fn.id
        name = id.name
        sym = id.sym
        fnty = id.typ
        paramty = fnty.params.mapIt(newLType(it, module))
        rety = if fnty.rety == Value.Unit: voidType() else: newLType(fnty.rety, module)
        retyhasregion = fnty.rety != Value.Unit and fnty.rety.hasRegion
    var
        cxt = module.cxt
        fnty2 = if retyhasregion:
                functionType(module.cxt.voidType, paramty & pointerType(rety))
            else:
                functionType(rety, paramty)
        fn2 = module.module.addFunction(name, fnty2)
    sym.val = fn2
    sym.lty = fnty2
    if fn.metadata.isSome:
        case fn.metadata.get.kind
        of MetadataKind.ImportLL:
            let fun1 = module.linkFuncs.filterIt(it.name == name)
            assert fun1.len == 1
            # let fun = fun1[0]
            # echo fun.typ
            # echo newLType(sym.typ, module)
            # echo fun.typ == newLType(sym.typ, module)
            return
        else:
            discard
    if fn.suite.isSome:
        module.enter fn2:
            for i in 0..<fn.param.params.len:
                let
                    pat = fn.param.params[i].pat
                    typ = pat.typ
                    val = fn2.param(i)
                setParam(pat, module, typ, val)
            var ret = fn.suite.get.codegen(module)
            if not ret.isNil:
                if retyhasregion:
                    discard module.curBuilder.store(ret, fn2.param(fn.param.params.len))
                    discard module.curBuilder.retVoid()
                else:
                    discard module.curBuilder.ret(ret)


proc codegen(self: IdentDefSection, module: Module, global: bool = false) =
    for e in self.iddefs:
        let
            pat = e.pat
            default = e.default.get
        codegen(pat, module, default.typ, default.codegen(module, global))
proc codegen(self: Statement, module: Module, global: bool = false): LValue =
    case self.kind
    of StatementKind.For:
        nil
    of StatementKind.While:
        nil
    of StatementKind.Loop:
        nil
    of StatementKind.LetSection, StatementKind.VarSection:
        self.iddefSection.codegen(module, global)
        nil
    of StatementKind.ConstSection:
        nil
    of StatementKind.TypeSection:
        nil
    of StatementKind.Asign:
        nil
    of StatementKind.Funcdef:
        self.fn.codegen(module, global)
        nil
    of StatementKind.Meta:
        let metadata = self.meta
        case metadata.kind
        of MetadataKind.Link:
            metadata.link(module)
        else:
            assert false
        nil
    of StatementKind.Discard:
        nil
    of StatementKind.Comments:
        nil
    of StatementKind.Expression:
        self.expression.codegen(module, global, lval=false)
    of StatementKind.Fail:
        nil

proc codegen*(self: Program, module: Module, global: bool = false) =
    # debug self.typ
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
    # debug module.module
