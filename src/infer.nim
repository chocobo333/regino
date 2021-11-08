
import strformat
import strutils
import sequtils
import options
import sets
import tables
import algorithm

import
    il,
    typeenvs,
    errors,
    relations,
    orders,
    utils


proc `<=`*(env: TypeEnv, t1, t2: ref Type): bool =
    if t1.kind == TypeKind.Bottom:
        true
    elif t2.kind == TypeKind.Top:
        true
    elif t1.kind != TypeKind.Var and t1 == t2:
        true
    else:
        env.typeOrder.anyIt((t1, t2) in it)
proc `<=?`*(env: TypeEnv, t1, t2: ref Type): Option[seq[Constraint]] =
    if t1.kind == TypeKind.Bottom:
        some newSeq[Constraint]()
    elif t2.kind == TypeKind.Top:
        some newSeq[Constraint]()
    elif t1.kind != TypeKind.Var and t1 == t2:
        some newSeq[Constraint]()
    elif t2.kind == TypeKind.Var:
        if t2 <= t1:
            none(seq[Constraint])
        else:
            some @[(t1, t2)]
    elif t1.kind == t2.kind:
        case t1.kind
        of TypeKind.Arrow:
            let
                paramty = t1.paramty.zip(t2.paramty).mapIt(env.`<=?`(it[1], it[0]))
                rety = env.`<=?`(t1.rety, t2.rety)
            if paramty.allIt(it.isSome) and rety.isSome:
                some paramty.mapIt(it.get).flatten & rety.get
            else:
                none(seq[Constraint])
        else:
            none(seq[Constraint])
    else:
        if env.typeOrder.anyIt((t1, t2) in it):
            some newSeq[Constraint]()
        else:
            none(seq[Constraint])

template setTypeEnv(env: TypeEnv): untyped =
    proc `<=`(t1, t2: ref Type): bool =
        env.`<=`(t1, t2)
    proc `<=?`(t1, t2: ref Type): Option[seq[Constraint]] =
        env.`<=?`(t1, t2)

proc lub(self: TypeEnv, t1, t2: ref Type): ref Type =
    setTypeEnv(self)
    if t1 <= t2:
        t1
    elif t2 <= t1:
        t2
    else:
        raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
proc glb(self: TypeEnv, t1, t2: ref Type): ref Type =
    setTypeEnv(self)
    if t1 <= t2:
        t2
    elif t2 <= t1:
        t1
    else:
        raise newException(TypeError, fmt"{t1} and {t2} can not be unified")

proc coerceRelation(self: var TypeEnv, t1, t2: ref Type) =
    if t1.kind == t2.kind and t1.kind in {TypeKind.Bottom..TypeKind.String}:
        discard
    else:
        self.constraints.add (t1, t2)
proc addTypeRelation(self: var TypeEnv, t1, t2: ref Type) =
    assert t1.kind == TypeKind.Typedesc
    assert t2.kind == TypeKind.Typedesc
    self.typeOrder[^1].add (t1.typ, t2.typ)

proc bindtv(self: TypeEnv, t1, t2: ref Type) =
    case t2.kind
    of TypeKind.Var:
        let symbol = t1.symbol
        t1[] = Type.Link(t2)[]
        t1.symbol = symbol
    of TypeKind.Link:
        self.bindtv(t1, t2.to)
    else:
        if t1.kind == TypeKind.Intersection:
            t1[] = t2[]
        else:
            let symbol = t1.symbol
            t1[] = t2[]
            t1.symbol = symbol
proc resolveRelation(self: var TypeEnv, t1, t2: ref Type) =
    # implies t1 < t2
    # setTypeEnv(self)
    template `<=`(t1, t2: ref Type): bool =
        self.`<=`(t1, t2)
    template `<=?`(t1, t2: ref Type): Option[seq[Constraint]] =
        self.`<=?`(t1, t2)
    # TODO: should change function name
    if t1.kind == TypeKind.Link:
        self.resolveRelation(t1.to, t2)
    if t2.kind == TypeKind.Link:
        self.resolveRelation(t1, t2.to)
    if t1 == t2:
        return
    if t1.kind == t2.kind:
        case t1.kind
        of TypeKind.Bottom..TypeKind.String:
            return
        of TypeKind.List:
            self.resolveRelation(t1.base, t2.base)
        of TypeKind.Tuple:
            if t1.types.len == t2.types.len:
                for (t1, t2) in t1.types.zip(t2.types):
                    self.resolveRelation(t1, t2)
            else:
                raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
        of TypeKind.Record:
            # TODO: implment for record
            raise newException(TypeError, "notimplemented")
        of TypeKind.Arrow:
            for (t1, t2) in t1.paramty.zip(t2.paramty):
                self.resolveRelation(t2, t1)
            self.resolveRelation(t1.rety, t2.rety)
        of TypeKind.Typedesc:
            self.resolveRelation(t1.typ, t2.typ)
        else:
            t1.tv.ub = self.lub(t1.tv.ub, t2.tv.ub)
            t2.tv.lb = self.glb(t1.tv.lb, t2.tv.lb)
            if t1.tv.ub == t1.tv.lb:
                self.bindtv(t1, t1.tv.ub)
            if t2.tv.ub == t2.tv.lb:
                self.bindtv(t2, t2.tv.ub)
            if t1.kind == TypeKind.Var and t2.kind == TypeKind.Var:
                self.tvconstraints.add (t1, t2)
    else:
        if t1.kind == TypeKind.Var:
            # TODO: if lb >= t2
            if t1.tv.lb <= t2 and t2 <= t1.tv.ub:
                t1.tv.ub = t2
            else:
                raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
            if t1.tv.lb == t2:
                self.bindtv(t1, t2)
        elif t2.kind == TypeKind.Var:
            if t2.tv.lb <= t1 and t1 <= t2.tv.ub:
                t2.tv.lb = t1
            else:
                raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
            if t2.tv.ub == t1:
                self.bindtv(t2, t1)
        elif t1.kind == TypeKind.Intersection:
            let l = t1.types.zip(t1.types.mapIt(it <=? t2)).filterIt(it[1].isSome).mapIt((it[0], it[1].get))
            case l.len
            of 0:
                raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
            of 1:
                let (t3, cons) = l[0]
                self.bindtv(t1, t3)
                self.constraints.add cons
                # TODO: add opppsite constraint
                if t3.kind == TypeKind.Arrow and t2.kind == TypeKind.Arrow:
                    self.constraints.add (t2.rety, t3.rety)
            else:
                self.bindtv(t1, Type.Intersection(l.mapIt(it[0])))
        elif t1 <= t2:
            echo "debug: ", t1, " <= ", t2
        else:
            echo t1
            echo t2
            echo t1.symbol
            echo t2.symbol
            echo self.constraints
            raise newException(TypeError, fmt"{t1} and {t2} can not be unified")
proc resolveRelations(self: var TypeEnv) =
    while self.constraints.len > 0:
        self.constraints.reverse
        while self.constraints.len > 0:
            let
                (t1, t2) = self.constraints.pop
            self.resolveRelation(t1, t2)
        (self.constraints, self.tvconstraints) = block:
            var
                l1: seq[Constraint]
                l2: seq[Constraint]
            for (t1, t2) in self.tvconstraints:
                if t1.kind == TypeKind.Var and t2.kind == TypeKind.Var:
                    l2.add (t1, t2)
                else:
                    l1.add (t1, t2)
            (l1, l2)

proc typeInfer*(self: ref Term, env: var TypeEnv, global: bool = false): ref Type =
    result = case self.kind
    of TermKind.Unit:
        Type.Unit
    of TermKind.Bool:
        Type.Bool
    of TermKind.Integer:
        Type.Integer
    of TermKind.Float:
        Type.Float
    of TermKind.Char:
        Type.Char
    of TermKind.String:
        Type.String
    of TermKind.Id:
        let
            syms = env.lookupId(self.name)
        if syms.len == 1:
            syms[0].ptyp.inst
        else:
            Type.Intersection(syms.mapIt(it.ptyp.inst))
    of TermKind.Lambda:
        Type.Unit
    of TermKind.List:
        Type.Unit
    of TermKind.Tuple:
        Type.Unit
    of TermKind.Record:
        Type.Unit
    of TermKind.Let:
        let
            tv = Type.Var
            id = self.iddef.id
            impl = self.iddef.default.get
            t1 = impl.typeInfer(env, global)
            sym = if t1.kind == TypeKind.Arrow:
                # PSymbol.Let(t1.gen(env), self, global)
                # TODO:
                PSymbol()
            else:
                PSymbol.Let(id, PolyType.ForAll(nullFtv, tv), impl, global)
            # sym = PSymbol.Let(t1.gen(env))
        id.typ = tv
        env.addIdent(id, sym)
        env.coerceRelation(t1, tv)
        env.coerceRelation(tv, t1)
        Type.Unit
    of TermKind.Var:
        let
            tv = Type.Var
            id = self.iddef.id
            impl = self.iddef.default.get
            t1 = impl.typeInfer(env, global)
            sym = if t1.kind == TypeKind.Arrow:
                # PSymbol.Let(t1.gen(env), self, global)
                # TODO:
                PSymbol()
            else:
                PSymbol.Let(id, PolyType.ForAll(nullFtv, tv), impl, global)
            # sym = PSymbol.Let(t1.gen(env))
        id.typ = tv
        env.addIdent(id, sym)
        env.coerceRelation(t1, tv)
        Type.Unit
    of TermKind.Const:
        Type.Unit
    of TermKind.Typedef:
        for typedef in self.typedefs:
            let
                id = typedef.id
                default = typedef.default.get
                t1 = default.typeInfer(env, global)
                sym = PSymbol.Typ(id, PolyType.ForAll(nullFtv, t1), default, global)
            id.typ = t1
            env.addIdent(id, sym)
        Type.Unit
    of TermKind.Funcdef:
        let
            paramty = self.fn.param.params.mapIt(it.typ.get.typeInfer(env, global))
            rety = self.fn.param.rety.typeInfer(env, global)
            metadata = self.fn.metadata
            tvs = paramty.mapIt(Type.Var())
            tv = Type.Var()
            fnty = Type.Arrow(tvs, tv)
            sym = PSymbol.Func(self.fn.id, PolyType.ForAll(nullFtv, fnty), self, global)
        self.fn.id.typ = fnty
        env.addIdent(self.fn.id, sym)
        for (p, v) in paramty.zip(tvs.mapIt(Type.Typedesc(it))):
            env.coerceRelation(v, p)
            env.coerceRelation(p, v)
        if rety.kind == TypeKind.Unit:
            env.coerceRelation(tv, rety)
            env.coerceRelation(rety, tv)
        else:
            env.coerceRelation(Type.TypeDesc(tv), rety)
            env.coerceRelation(rety, Type.TypeDesc(tv))
        env.pushScope
        for (iddef, tv) in self.fn.param.params.zip(tvs):
            let
                sym = PSymbol.Let(iddef.id, PolyType.ForAll(nullFtv, tv), iddef.default.get(Term.Unit), global)
            iddef.id.typ = tv
            env.addIdent(iddef.id, sym)
        let
            inferedRety = self.fn.body.typeInfer(env)
        env.popScope
        if metadata.isSome:
            case metadata.get.kind
            of MetadataKind.ImportLL:
                env.coerceRelation(inferedRety, Type.Unit)
                env.coerceRelation(Type.Unit, inferedRety)  # ?
            of MetadataKind.Subtype:
                assert paramty.len == 1, "converter must take only one argument."
                env.addTypeRelation(paramty[0], rety)
                env.coerceRelation(inferedRety, tv)
            else:
                discard
        else:
            env.coerceRelation(inferedRety, tv)
        Type.Unit
    of TermKind.If:
        let
            conds = self.`elif`.mapIt(it[0].typeInfer(env, global))
            thents = self.`elif`.mapIt(it[1].typeInfer(env, global))
            elset = self.`else`.typeInfer(env, global)
            tv = Type.Var
        for cond in conds:
            env.coerceRelation(cond, Type.Bool)
            env.coerceRelation(Type.Bool, cond)
        for thent in thents:
            env.coerceRelation(thent, tv)
        env.coerceRelation(elset, tv)
        tv
    of TermKind.When:
        Type.Unit
    of TermKind.Case:
        Type.Unit
    of TermKind.While:
        Type.Unit
    of TermKind.For:
        Type.Unit
    of TermKind.Loop:
        Type.Unit
    of TermKind.Block:
        Type.Unit
    of TermKind.Asign:
        Type.Unit
    of TermKind.Typeof:
        Type.Typedesc(self.term.typeInfer(env, global))
    of TermKind.Discard:
        discard self.term.typeInfer(env)
        Type.Unit
    of TermKind.Apply:
        let
            callee = self.callee.typeInfer(env, global)
            args = self.args.mapIt(it.typeInfer(env, global))
            tv = Type.Var()
        # env.coerceRelation(Type.Arrow(args, tv), callee) # ?
        env.coerceRelation(callee, Type.Arrow(args, tv))
        # TODO: if callee is not of Interseciion type, add tv < callee.rety
        if callee.kind == TypeKind.Arrow:
            env.coerceRelation(tv, callee.rety)
        tv
    of TermKind.Meta:
        if not self.metadata.param.isNil:
            discard self.metadata.param.typeInfer(env, global)
        Type.Unit
    of TermKind.Seq:
        let
            terms = self.terms.mapIt(it.typeInfer(env, global))
        for e in terms[0..^2]:
            env.coerceRelation(e, Type.Unit)
            env.coerceRelation(Type.Unit, e)
        terms[^1]
    self.typ = result

proc typeCheck(self: ref Term, env: TypeEnv): seq[Error] =
    if self.typ.kind == TypeKind.Var:
        if self.typ.tv.lb.compilable:
            env.bindtv(self.typ, self.typ.tv.lb)
        else:
            return @[TypeError.Undeciable]
    proc check(self: ref Term, typ2: ref Type): seq[Error] =
        if self.typ == typ2:
            @[]
        else:
            @[InternalError.new]
    case self.kind
    of TermKind.Unit:
        self.check(Type.Unit)
    of TermKind.Bool:
        self.check(Type.Bool)
    of TermKind.Integer:
        self.check(Type.Integer)
    of TermKind.Float:
        self.check(Type.Float)
    of TermKind.Char:
        self.check(Type.Char)
    of TermKind.String:
        self.check(Type.String)
    of TermKind.Id:
        assert self.typ.symbol.isSome
        # self.typ.symbol.get.instances[self.typ] = Symbol()
        @[]
    of TermKind.Lambda:
        self.check(Type.Unit)
    of TermKind.List:
        self.check(Type.Unit)
    of TermKind.Tuple:
        self.check(Type.Unit)
    of TermKind.Record:
        self.check(Type.Unit)
    of TermKind.Let:
        let
            id = self.iddef.id
            impl = self.iddef.default.get
            ret = id.typeCheck(env) & impl.typeCheck(env)
        assert impl.typ == id.typ
        self.check(Type.Unit) & ret
    of TermKind.Var:
        # let
        #     tv = Type.Var
        #     id = self.iddef.id
        #     impl = self.iddef.default.get
        #     t1 = impl.typeInfer(env, global)
        #     sym = if t1.kind == TypeKind.Arrow:
        #         # PSymbol.Let(t1.gen(env), self, global)
        #         # TODO:
        #         PSymbol()
        #     else:
        #         PSymbol.Let(id, PolyType.ForAll(nullFtv, tv), impl, global)
        #     # sym = PSymbol.Let(t1.gen(env))
        # id.typ = tv
        # env.addIdent(id, sym)
        # env.coerceRelation(t1, tv)
        self.check(Type.Unit)
    of TermKind.Const:
        self.check(Type.Unit)
    of TermKind.Typedef:
        # for typedef in self.typedefs:
        #     let
        #         id = typedef.id
        #         default = typedef.default.get
        #         t1 = default.typeInfer(env, global)
        #         sym = PSymbol.Typ(id, PolyType.ForAll(nullFtv, t1), default, global)
        #     id.typ = t1
        #     env.addIdent(id, sym)
        self.check(Type.Unit)
    of TermKind.Funcdef:
        let
            # paramty = self.fn.param.params.mapIt(it.typ.get.typeInfer(env, global))
            # rety = self.fn.param.rety.typeInfer(env, global)
            ret = self.fn.param.params.mapIt(it.typ.get.typeCheck(env)).flatten &
                self.fn.param.params.mapIt(it.id.typeCheck(env)).flatten &
                self.fn.id.typeCheck(env) & self.fn.param.rety.typeCheck(env)
        #     metadata = self.fn.metadata
        #     tvs = paramty.mapIt(Type.Var())
        #     tv = Type.Var()
        #     fnty = Type.Arrow(tvs, tv)
        #     sym = PSymbol.Func(self.fn.id, PolyType.ForAll(nullFtv, fnty), self, global)
        # env.addIdent(self.fn.id, sym)
        # for (p, v) in paramty.zip(tvs.mapIt(Type.Typedesc(it))):
        #     env.coerceRelation(v, p)
        #     env.coerceRelation(p, v)
        # if rety.kind == TypeKind.Unit:
        #     env.coerceRelation(tv, rety)
        #     env.coerceRelation(rety, tv)
        # else:
        #     env.coerceRelation(Type.TypeDesc(tv), rety)
        #     env.coerceRelation(rety, Type.TypeDesc(tv))
        # env.pushScope
        # for (iddef, tv) in self.fn.param.params.zip(tvs):
        #     let
        #         sym = PSymbol.Let(iddef.id, PolyType.ForAll(nullFtv, tv), iddef.default.get(Term.Unit), global)
        #     iddef.id.typ = tv
        #     env.addIdent(iddef.id, sym)
        # let
        #     inferedRety = self.fn.body.typeInfer(env)
        # env.popScope
        # if metadata.isSome:
        #     case metadata.get.kind
        #     of MetadataKind.ImportLL:
        #         env.coerceRelation(inferedRety, Type.Unit)
        #         env.coerceRelation(Type.Unit, inferedRety)  # ?
        #     of MetadataKind.Subtype:
        #         assert paramty.len == 1, "converter must take only one argument."
        #         env.addTypeRelation(paramty[0], rety)
        #     else:
        #         discard
        # else:
        #     env.coerceRelation(inferedRety, tv)
        self.check(Type.Unit) & ret
    of TermKind.If:
        let
            conds = self.`elif`.mapIt(it[0].typeCheck(env))
            thens = self.`elif`.mapIt(it[1].typeCheck(env))
            elset = self.`else`.typeCheck(env)
            condtypes = self.`elif`.mapIt(it[0].typ)
            thentypes = self.`elif`.mapIt(it[1].typ)
            elsetype = self.`else`.typ
        for cond in condtypes:
            assert cond == Type.Bool
        # for thent in thents:
        #     env.coerceRelation(thent, tv)
        # env.coerceRelation(elset, tv)
        # tv
        conds.flatten & thens.flatten & elset
    of TermKind.When:
        self.check(Type.Unit)
    of TermKind.Case:
        self.check(Type.Unit)
    of TermKind.While:
        self.check(Type.Unit)
    of TermKind.For:
        self.check(Type.Unit)
    of TermKind.Loop:
        self.check(Type.Unit)
    of TermKind.Block:
        self.check(Type.Unit)
    of TermKind.Asign:
        self.check(Type.Unit)
    of TermKind.Typeof:
        self.check(Type.Typedesc(self.term.typ)) & self.term.typeCheck(env)
    of TermKind.Discard:
        self.check(Type.Unit) & self.term.typeCheck(env)
    of TermKind.Apply:
        self.callee.typeCheck(env) &  self.args.mapIt(it.typeCheck(env)).flatten
        # let
        #     callee = self.callee.typeInfer(env, global)
        #     args = self.args.mapIt(it.typeInfer(env, global))
        #     tv = Type.Var()
        # # env.coerceRelation(Type.Arrow(args, tv), callee) # ?
        # env.coerceRelation(callee, Type.Arrow(args, tv))
        # tv
    of TermKind.Meta:
        # if not self.metadata.param.isNil:
        #     discard self.metadata.param.typeInfer(env, global)
        self.check(Type.Unit)
    of TermKind.Seq:
        if self.terms.len > 1:
            var ret = self.terms.mapIt(it.typeCheck(env)).flatten
            var check: bool = true
            let
                types = self.terms.mapIt(it.typ)
                typ = types[^1]
            check = types[0..^2].mapIt(it == Type.Unit).foldl(a and b)
            check = check and self.typ.kind != TypeKind.Var and typ.kind != TypeKind.Var
            # self.typ == typ
            ret
        else:
            var
                ret = self.terms[0].typeCheck(env)
            let
                typ = self.terms[0].typ
            ret.add if self.typ.kind != TypeKind.Var and typ.kind != TypeKind.Var:
                @[]
            else:
                @[TypeError.Undeciable]
            ret


proc infer*(self: ref Term, env: var TypeEnv, global: bool = false): ref Type =
    result = self.typeInfer(env, global)
    env.coerceRelation(result, Type.Integer)
    env.resolveRelations()
    let errs = self.typeCheck(env)
    if errs.len > 0:
        var err: ref Error = new(Error)
        err[] = errs[0]
        raise err

