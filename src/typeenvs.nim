
import tables
import sets
import sequtils
import hashes
import sugar
import options

import il
import errors
import utils


type
    Substitution* = Table[TypeVar, ref Value]
    TypeEnv* = ref object
        scope*: Scope
        tvs*: HashSet[ref Value]
        constraints*: seq[Constraint]
        tvconstraints*: seq[Constraint]
        interconstraints*: seq[Constraint]
    Substituable* = concept T, var t
        apply(Substitution, T)
        t.ftv() is HashSet[TypeVar]
    Constraint* = (ref Value, ref Value)   # for t1 <= t2


proc hash*(self: Constraint): Hash =
    result = self[0].hash
    result = result !& self[1].hash
    result = !$result

proc Var*(_: typedesc[Value], env: TypeEnv): ref Value =
    result = new Value
    result[] = Value(kind: ValueKind.Var, tv: newTypeVar())
    env.tvs.incl result

proc pushScope*(env: TypeEnv, scope: Scope) =
    assert scope.parent == env.scope
    env.scope = scope

proc popScope*(env: TypeEnv): Scope {.discardable.} =
    result = env.scope
    env.scope = env.scope.parent

proc lookupId*(self: TypeEnv, name: string, kinds: set[SymbolKind] = {SymbolKind.low..SymbolKind.high}): seq[Symbol] =
    var
        tmp: set[SymbolKind]
    for scope in self.scope:
        if name in scope.syms:
            let
                syms = scope.syms[name].filterIt(it.kind notin tmp and it.kind in kinds)
                vars = syms.filterIt(it.kind in {SymbolKind.Let, SymbolKind.Var, SymbolKind.Const})
                types = syms.filterIt(it.kind == SymbolKind.Typ)
                funcs = syms.filterIt(it.kind == SymbolKind.Func)
            if vars.len > 0:
                result.add vars[^1]
            if types.len > 0:
                result.add types[^1]
            result.add funcs
            for k in syms.mapIt(it.kind):
                if k != SymbolKind.Func:
                    tmp.incl k
proc lookupConst*(self: TypeEnv, name: string): seq[Symbol] =
    for scope in self.scope:
        if name in scope.consts:
            result.add scope.consts[name]

proc lookupConverter*(self: Scope, t1, t2: ref Value): Ident =
    for scope in self:
        if (t1, t2) in scope.converters:
            return scope.converters[(t1, t2)]
    raise newException(TypeError, "")
proc lookupConverter*(self: TypeEnv, t1, t2: ref Value): Ident =
    self.scope.lookupConverter(t1, t2)

proc addIdent*(self: TypeEnv, id: Ident, sym: Symbol) =
    let
        name = id.name
    if name in self.scope.syms:
        self.scope.syms[name].add sym
    else:
        self.scope.syms[name] = @[sym]
proc addConst*(self: TypeEnv, id: Ident, sym: Symbol) =
    assert sym.kind in {SymbolKind.Const, SymbolKind.Typ}
    let
        name = id.name
    if name in self.scope.consts:
        self.scope.consts[name].add sym
    else:
        self.scope.consts[name] = @[sym]

proc newTypeEnv*(scope: Scope): TypeEnv =
    TypeEnv(
        scope: scope
    )

# let
#     nullSubst* = initTable[TypeVar, ref Value]()
#     nullFtv* = initHashSet[TypeVar]()

# TODO: Assert that `Type`, `PolyType` and so on are Substituable.
# assert

# proc apply*(self: Substitution, t: ref Value): ref Value =
#     case t.kind
#     of ValueKind.Bottom..ValueKind.String:
#         discard
#     # of ValueKind.List:
#     #     t.elem = self.apply t.elem
#     of ValueKind.Pair:
#         t.first = self.apply t.first
#         t.second = self.apply t.second
#     of ValueKind.Record:
#         for key in t.members.keys:
#             t.members[key] = self.apply t.members[key]
#     # of ValueKind.Tuple, ValueKind.Intersection:
#     of ValueKind.Intersection, ValueKind.Union:
#         t.types = toSeq(t.types.items).mapIt(self.apply it).toHashSet
#     # of ValueKind.Record:
#     #     for i in 0..<t.idtypes.len:
#     #         t.idtypes[i] = (t.idtypes[i][0], self.apply t.idtypes[i][1])
#     # of ValueKind.Arrow:
#     #     for i in 0..<t.paramty.len:
#     #         t.paramty[i] = self.apply t.paramty[i]
#     #     t.rety = self.apply t.rety
#     of ValueKind.Pi:
#         for i in 0..<t.genty.len:
#             t.genty[i][1] = self.apply t.genty[i][1]
#         for i in 0..<t.paramty.len:
#             t.paramty[i] = self.apply t.paramty[i]
#         t.rety = self.apply t.rety
#     # of ValueKind.Sigma:
#     #     t.first = self.apply t.first
#     #     t.second = self.apply t.second
#     of ValueKind.Var:
#         if t.tv in self:
#             # TODO: check overloads
#             return self[t.tv]
#         else:
#             t.tv.ub = self.apply t.tv.ub
#             t.tv.lb = self.apply t.tv.lb
#     # of ValueKind.Typedesc, ValueKind.Distinct:
#     of ValueKind.Typedesc:
#         t.`typedesc` = self.apply(t.`typedesc`)
#     of ValueKind.Link:
#         t.to = self.apply(t.to)
#     of ValueKind.Gen:
#         discard
#     t

# proc inst*(typ: PolyType): ref Value =
#     result = case typ.kind
#     of PolyTypeKind.ForAll:
#         let
#             gen = toSeq(typ.gen.items)
#             s = gen.zip(gen.mapIt(Value.Var())).toTable
#         s.apply(typ.typ)
#     of PolyTypeKind.Intersection:
#         Value.Intersection(typ.types.map(inst))
proc inst*(typ: ref Value, env: TypeEnv, subs: Table[GenType, ref Value] = initTable[GenType, ref Value]()): ref Value =
    let sym = typ.symbol
    result = case typ.kind
    of ValueKind.Bottom..ValueKind.String:
        typ.deepCopy
    # of ValueKind.List:
    #     nil
    of ValueKind.Pair:
        Value.Pair(typ.first.inst(env, subs), typ.second.inst(env, subs))
    # of ValueKind.Tuple:
    #     nil
    of ValueKind.Record:
        Value.Record(toSeq(typ.members.pairs).mapIt((it[0], it[1].inst(env, subs))))
    of ValueKind.Pi:
        let
            subs = typ.genty.map(it => (block:
                let t = Value.Var(env)
                t.tv.ub = it[1].gen.ub
                (it[1].gen, t)
            ))
            ret = Value.Arrow(typ.paramty.mapIt(it.inst(env, subs.toTable)), typ.rety.inst(env, subs.toTable))
        ret.gentyinst = subs.mapIt(it[1])
        ret
    # of ValueKind.Sigma:
    #     nil
    of ValueKind.Typedesc:
        Value.Typedesc(typ.`typedesc`.inst(env, subs))
    # of ValueKind.Distinct:
    #     nil
    of ValueKind.Var:
        typ
    of ValueKind.Intersection:
        Value.Intersection(typ.types.map(it => it.inst(env, subs)))
    of ValueKind.Union:
        Value.Union(typ.types.map(it => it.inst(env, subs)))
    of ValueKind.Link:
        Value.Link(typ.to.inst(env, subs))
    of ValueKind.Gen:
        if typ.gen in subs:
            subs[typ.gen]
        else:
            typ
    result.symbol = sym

proc impl*(self: Term): Term =
    self.typ.symbol.get.impl
proc genty*(self: Term): seq[ref Value] =
    self.typ.gentyinst
proc inst*(t: Term): Term
proc inst*(iddef: IdentDef): IdentDef =
    IdentDef(
        pat: iddef.pat.deepCopy,
        typ: iddef.typ.map(inst),
        default: iddef.default.map(inst)
    )
proc inst*(param: FunctionParam): FunctionParam =
    FunctionParam(
        gen: param.gen.map(inst),
        params: param.params.map(inst),
        rety: param.rety.inst
    )
proc inst*(metadata: Metadata): Metadata =
    case metadata.kind
    of MetadataKind.Link..MetadataKind.Subtype:
        Metadata(kind: metadata.kind, param: metadata.param.map(inst))
    of MetadataKind.Userdef:
        Metadata(kind: MetadataKind.Userdef, name: metadata.name, param: metadata.param.map(inst))
proc inst*(body: Body): Body =
    Body(
        term: body.term.inst,
        scope: newScope(body.scope.parent)
    )
proc inst*(fn: Function): Function =
    Function(
        id: fn.id.inst,
        param: fn.param.inst,
        metadata: fn.metadata.map(inst),
        body: fn.body.inst
    )

proc inst*(t: Term): Term =
    result = case t.kind
    of TermKind.Failed:
        Term.Failed
    of TermKind.bottom:
        Term.bottom
    of TermKind.`()`:
        Term.unit
    of TermKind.Unit:
        Term.Unit
    of TermKind.U:
        Term.U(t.level)
    # of TermKind.Bool:
    #     $self.boolval
    of TermKind.Integer:
        Term.Integer(t.intval)
    of TermKind.Float:
        Term.Float(t.floatval)
    of TermKind.Char:
        Term.Char(t.charval)
    of TermKind.String:
        Term.String(t.strval)
    of TermKind.Id:
        Term.Id(t.name)
    # of TermKind.Lambda:
    #     fmt"λ{self.param}.{self.body}"
    # of TermKind.List:
    #     let s = self.terms.map(`$`).join(", ")
    #     fmt"[{s}]"
    of TermKind.Tuple:
        Term.Tuple(t.terms.mapIt(it.inst))
    of TermKind.Record:
        Term.Record(t.members.map(it => it.inst))
    of TermKind.Let:
        Term.Let(t.iddef.inst)
    # of TermKind.Var:
    #     fmt"var {self.iddef}"
    of TermKind.Const:
        Term.Const(t.iddef.inst)
    # of TermKind.Typedef:
    #     let s = self.typedefs.map(`$`).join("\n")
    #     &"type\n{s.indent(2)}"
    of TermKind.Funcdef, TermKind.FuncdefInst:
        Term.Funcdef(t.fn.inst)
    # of TermKind.If:
    #     let
    #         elift = self.`elif`.mapIt(&"elif {it[0]}:\n{($it[1]).indent(2)}").join("\n")[2..^1]
    #         elset = ($self.`else`).indent(2)
    #     &"{elift}\nelse:\n{elset}"
    # of TermKind.When:
    #     ""
    of TermKind.Case:
        Term.Case()
    # of TermKind.While:
    #     ""
    # of TermKind.For:
    #     ""
    # of TermKind.Loop:
    #     let s = $self.body
    #     &"loop {self.label}\n{s.indent(2)}"
    # of TermKind.Block:
    #     let s = $self.body
    #     &"block {self.label}\n{s.indent(2)}"
    # of TermKind.Asign:
    #     fmt"{self.pat} = {self.val}"
    of TermKind.Typeof:
        Term.Typeof(t.term.inst)
    of TermKind.Discard:
        Term.Discard(t.term.inst)
    of TermKind.Apply:
        Term.Apply(t.callee.inst, t.args.mapIt(it.inst))
    # of TermKind.Projection:
    #     fmt"{self.container}.{self.index}"
    of TermKind.Meta:
        Term.Meta(t.metadata.inst)
    of TermKind.Seq:
        Term.Seq(t.terms.mapIt(it.inst))
    result.typ = nil

proc implInst*(self: Term): Term =
    assert self.kind == TermKind.Id
    let
        gentyinst = self.genty
        impl = self.impl
    result = impl.inst
    assert result.kind == TermKind.Funcdef
    result = Term(kind: TermKind.FuncdefInst, fn: result.fn)
    result.fn.param.gen = @[]
# proc compose(a, b: Substitution): Substitution =
#     result = initTable[TypeVar, ref Value]()
#     for key, value in b:
#         result[key] = a.apply(value)
#     for key, value in a:
#         if key notin result:
#             result[key] = value
#         else:
#             if value.kind == ValueKind.Var:
#                 if value.tv notin result:
#                     result[value.tv] = Value.Var(key)
#                 else:
#                     raise newException(TypeError, "")
# proc `@`*(a, b: Substitution): Substitution =
#     compose(a, b)
