
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
#         scopes*: Scope
#     Scope* = ref object
#         parent*: Scope
#         vars*: Table[string, (LType, LValue)]

# proc newScope(parent: Scope = nil): Scope =
#     Scope(
#         parent: parent,
#         vars: initTable[string, (LType, LValue)]()
#     )
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
# proc pushScope(self: Module) =
#     self.scopes = newScope(self.scopes)
# proc popScope(self: Module) =
#     self.scopes = self.scopes.parent
# proc extend(self: Scope, name: string, typ: LType, val: LValue) =
#     self.vars[name] = (typ, val)
# proc lookup(self: Scope, name: string): (LType, LValue) =
#     var scope = self
#     while not scope.isNil:
#         if name in scope.vars:
#             return scope.vars[name]
#         scope = scope.parent
#     assert false, fmt"{name} was not declared"

var strtyp: LType
proc newLType(typ: ref Value, module: Module): LType =
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
            paramty = typ.paramty.mapIt(it.newLType(module))
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


proc builtin*(self: Module) =
    discard

proc sym(self: Term): Impl =
    if self.typ in self.typ.symbol.get.instances:
        result = self.typ.symbol.get.instances[self.typ]
    else:
        result = Impl(instance: none(Term))
        self.typ.symbol.get.instances[self.typ] = result
proc `sym=`*(self: Term, sym: Impl) =
    self.typ.symbol.get.instances[self.typ] = sym
proc paramty(self: Function): seq[ref Value] =
    self.id.typ.paramty
proc rety(self: Function): ref Value =
    self.id.typ.rety
proc codegen*(self: Term, module: Module, global: bool = false, lval: bool = false): LValue =
    proc Let(self: Term, typ: ref Value, val: LValue) =
        case self.kind
        of TermKind.Literal:
            discard
        of TermKind.Id:
            let
                id = self
                sym = id.sym
                name = id.name
                typ = newLType(typ, module)
                p = if global: module.module.newGlobal(typ, name) else: module.curBuilder.alloca(typ, name)
            sym.val = p
            sym.lty = typ
            discard module.curBuilder.store(val, p, $self)
        of TermKind.Tuple:
            assert typ.kind == ValueKind.Pair
            case self.terms.len
            of 0:
                discard
            of 1:
                Let(self.terms[0], typ.first, module.curBuilder.extractvalue(val, 0, $self))
            of 2:
                Let(self.terms[0], typ.first, module.curBuilder.extractvalue(val, 0, $self))
                Let(self.terms[1], typ.second, module.curBuilder.extractvalue(val, 1, $self))
            else:
                Let(self.terms[0], typ.first, module.curBuilder.extractvalue(val, 0, $self))
                Let(Term.Tuple(self.terms[1..^1]), typ.second, module.curBuilder.extractvalue(val, 1, $self))
        of TermKind.Record:
            for (i, key) in toSeq(self.members.keys).pairs:
                Let(self.members[key], typ.members[key], module.curBuilder.extractvalue(val, i, $self & "." & key))
        of TermKind.Us:
            discard
        else:
            assert false, "notimplemented"
    case self.kind
    of TermKind.`()`:
        nil
    of TermKind.Unit:
        nil
    of TermKind.Bool:
        let
            b = self.boolval
            boolty = newLType(Value.Bool, module)
        boolty.constInt(if b: 1 else: 0)
    of TermKind.Integer:
        let
            i = self.intval
            bits = self.bits
            intty = newLType(Value.Integer(bits), module)
        intty.constInt(int i)
    of TermKind.Float:
        let
            f = self.floatval
            floatty = newLType(Value.Float, module)
        floatty.constReal(f)
    of TermKind.String:
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
    of TermKind.Id:
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
    of TermKind.Tuple:
        proc makeTuple(typ: ref Value, terms: seq[Term]): LValue =
            if typ.second.kind == il.ValueKind.Pair:
            # if typ.second.kind == il.ValueKind.Sigma:
                var ret = module.curBuilder.insertvalue(typ.newLType(module).undef, makeTuple(typ.second, terms[1..^1]), 1, $terms[1..^1])
                module.curBuilder.insertvalue(ret, terms[0].codegen(module, global), 0, $terms[0])
            else:
                assert terms.len == 2, ""
                var ret = module.curBuilder.insertvalue(typ.newLType(module).undef, terms[1].codegen(module, global), 1, $terms[1])
                module.curBuilder.insertvalue(ret, terms[0].codegen(module, global), 0, $terms[0])

        makeTuple(self.typ, self.terms)
        # let
        #     typ = block:
        #         var
        #             typ = self.typ
        #             elemtyp: seq[LType]
        #         while typ.kind == il.ValueKind.Pair:
        #             elemtyp.add typ.first.newLType(module)
        #             typ = typ.second
        #         elemtyp.add typ.newLType(module)
        #         module.cxt.createStruct(
        #             elemtyp
        #         )
        # var ret: LValue = typ.undef
        # for (i, term) in self.seqval.pairs:
        #     ret = module.curBuilder.insertvalue(ret, term.codegen(module, global), i, $term)
        # module.curBuilder.bitcast(ret, self.typ.newLType(module))
    of TermKind.Record:
        let
            typ = self.typ.newLType(module)
        var
            ret: LValue = typ.undef
        for (i, term) in toSeq(self.members.values).pairs:
            ret = module.curBuilder.insertvalue(ret, term.codegen(module, global), i, $term)
        ret
    of TermKind.Let, TermKind.Var:
        # for iddef in self.iddefs:
        let
            iddef = self.iddef
            pat = iddef.pat
            default = iddef.default.get
        Let(pat, default.typ, default.codegen(module, global))
        nil
    # of TermKind.Var:
    #     let
    #         id = self.iddef.id
    #         sym = id.sym
    #         name = id.name
    #         default = self.iddef.default.get
    #         typ = newLType(default.typ, module)
    #         p = module.curBuilder.alloca(typ, name)
    #     sym.val = p
    #     sym.lty = typ
    #     discard module.curBuilder.store(default.codegen(module, global), p, $self)
    #     nil
    of TermKind.Const:
        # for iddef in self.iddefs:
        let
            iddef = self.iddef
            pat = iddef.pat
            default = iddef.default.get
        if pat.typ.kind != ValueKind.U:
            Let(pat, default.typ, default.codegen(module, global))
        nil
    of TermKind.FuncDef, TermKind.FuncdefInst:
        if self.fn.param.gen.len > 0:
            for val in self.fn.id.typ.symbol.get.instances.keys:
                assert self.fn.id.typ.symbol.get.instances[val].instance.isSome
                self.fn.id.typ.symbol.get.instances[val].val = self.fn.id.typ.symbol.get.instances[val].instance.get.codegen(module)
                self.fn.id.typ.symbol.get.instances[val].lty = newLType(val, module)
            return nil
        let
            fn = self.fn
            id = fn.id
            name = id.name
            sym = id.sym
            paramty = fn.paramty.mapIt(newLType(it, module))
            rety = if fn.rety == Value.Unit: voidType() else: newLType(fn.rety, module)
            retyhasregion = fn.rety != Value.Unit and fn.rety.hasRegion
        var
            cxt = module.cxt
            fnty = if retyhasregion:
                functionType(module.cxt.voidType, paramty & pointerType(rety))
            else:
                functionType(rety, paramty)
        var
            fn2 = module.module.addFunction(name, fnty)
        sym.val = fn2
        sym.lty = fnty
        if fn.metadata.isSome:
            case fn.metadata.get.kind
            of MetadataKind.ImportLL:
                let fun1 = module.linkFuncs.filterIt(it.name == name)
                assert fun1.len == 1
                # let fun = fun1[0]
                # echo fun.typ
                # echo newLType(sym.typ, module)
                # echo fun.typ == newLType(sym.typ, module)
                return nil
            else:
                discard
        var
            tmpBB = module.curBB
            tmpFun = module.curFun
            bb = fn2.appendBasicBlock("entry", cxt)
        module.curFun = fn2
        module.curBuilder.atEndOf(bb)
        module.curBB = bb
        proc Param(self: Term, typ: ref Value, val: LValue) =
            case self.kind
            of TermKind.Literal:
                discard
            of TermKind.Id:
                let
                    id = self
                    name = id.name
                    sym = id.sym
                    typ = newLType(typ, module)
                    p = module.curBuilder.alloca(typ, name)
                sym.val = p
                sym.lty = typ
                discard module.curBuilder.store(val, p, name)
            of TermKind.Tuple:
                assert typ.kind == ValueKind.Pair
                case self.terms.len
                of 0:
                    discard
                of 1:
                    Param(self.terms[0], typ.first, module.curBuilder.extractvalue(val, 0, $self))
                of 2:
                    Param(self.terms[0], typ.first, module.curBuilder.extractvalue(val, 0, $self))
                    Param(self.terms[1], typ.second, module.curBuilder.extractvalue(val, 1, $self))
                else:
                    Param(self.terms[0], typ.first, module.curBuilder.extractvalue(val, 0, $self))
                    Param(Term.Tuple(self.terms[1..^1]), typ.second, module.curBuilder.extractvalue(val, 1, $self))
            of TermKind.Record:
                for (i, key) in toSeq(self.members.keys).pairs:
                    Param(self.members[key], typ.members[key], module.curBuilder.extractvalue(val, i, $self & "." & key))
            of TermKind.Us:
                discard
            else:
                assert false, "notimplemented"
        for i in 0..<fn.param.params.len:
            let
                pat = fn.param.params[i].pat
                typ = fn.paramty[i]
                val = fn2.param(i)
            Param(pat, typ, val)
        # for i in 0..<fn.param.params.len:
        #     let
        #         id = fn.param.params[i].id
        #         name = id.name
        #         sym = id.sym
        #         typ = paramty[i]
        #         p = module.curBuilder.alloca(typ, name)
        #     sym.val = p
        #     sym.lty = typ
        #     discard module.curBuilder.store(fn2.param(i), p, name)
        var ret = fn.body.term.codegen(module)
        if not ret.isNil:
            if retyhasregion:
                discard module.curBuilder.store(ret, fn2.param(fn.param.params.len))
                discard module.curBuilder.retVoid()
            else:
                discard module.curBuilder.ret(ret)
        module.curBB = tmpBB
        module.curFun = tmpFun
        if not tmpBB.isNil:
            module.curBuilder.atEndOf(tmpBB)
        if self.kind == TermKind.Funcdef:
            nil
        else:
            fn2
    of TermKind.FunctionInst:
        self.sym.val
    of TermKind.Apply:
        let
            callee = self.callee
            args = self.args
            callee2 = codegen(callee, module)
            args2 = args.mapIt(codegen(it, module))
            rety = newLType(self.typ, module)
        # TODO: check returning void
        # module.curBuilder.call(callee2, args2, $self)
        if self.typ.hasRegion:
            let ret = module.curBuilder.alloca(rety, "*" & $self)
            discard module.curBuilder.call(callee2, args2 & ret)
            module.curBuilder.load(rety, ret, $self)
        else:
            module.curBuilder.call(callee2, args2)
    # of TermKind.Projection:
    #     let
    #         container = self.container
    #         index = self.index
    #     module.curBuilder.extractvalue(container.codegen(module), index, $self)
    of TermKind.If:
        var
            thenbs = self.`elif`.mapIt(module.curFun.appendBasicBlock("then"))
            elseb = module.curFun.appendBasicBlock("else")
            ifcont = module.curFun.appendBasicBlock("ifcont")

            thenvs: seq[LValue] = @[]

        for i in 0..<thenbs.len:
            if i == thenbs.len-1:
                discard module.curBuilder.condBr(self.`elif`[i][0].codegen(module), thenbs[i], elseb)
            else:
                var elifb = module.curFun.appendBasicBlock("elif")
                discard module.curBuilder.condBr(self.`elif`[i][0].codegen(module), thenbs[i], elifb)
                module.curBuilder.atEndOf(elifb)
                module.curBB = elifb
        for i in 0..<thenbs.len:
            module.curBuilder.atEndOf(thenbs[i])
            module.curBB = thenbs[i]
            let thenv = self.`elif`[i][1].term.codegen(module)
            thenvs.add thenv
            module.curBuilder.br(ifcont)

        module.curBuilder.atEndOf(elseb)
        module.curBB = elseb
        let elsev = self.`else`.term.codegen(module)
        module.curBuilder.br(ifcont)

        module.curBuilder.atEndOf(ifcont)
        module.curBB = ifcont
        if self.typ == Value.Unit:
            nil
        else:
            let phi = module.curBuilder.phi(newLType(self.typ, module), "")
            phi.addIncoming(thenvs.zip(thenbs) & @[(elsev, elseb)])
            phi
    of TermKind.Asign:
        let
            sym = self.pat.sym
            name = self.pat.name
        discard module.curBuilder.store(self.val.codegen(module, global), self.pat.codegen(module, global, lval=true), $self)
        nil
    of TermKind.Malloc:
        let
            size = module.targetdata.sizeofT(newLType(self.typ, module)) div 8
            p = module.curBuilder.call(module.malloc, @[module.cxt.intType(32).constInt((size * self.mallocsize.intval.uint).int)])
        module.curBuilder.bitcast(p, newLType(self.typ, module))
    of TermKind.Discard:
        discard self.term.codegen(module)
        nil
    of TermKind.Seq:
        var ret: LValue = nil
        for e in self.terms:
            ret = e.codegen(module, global)
        ret
    of TermKind.Meta:
        nil
    else:
        echo self
        assert false, "notimplemented"
        nil
