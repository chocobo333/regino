
import sets

import ir
import generators

type
    Constraint = (Type, Type)
    TypeEnv* = ref object
        id_gen: Generator[VarId]
        vars*: HashSet[TypeVar]
        constraints*: seq[Constraint]
proc newVar*(self: TypeEnv): TypeVar =
    let id = self.id_gen.get()
    result = TypeVar(
        kind: TypeVarKind.Var,
        id: id,
        ub: Type.Unit,
        lb: Type.Bottom
    )
    self.vars.incl result
proc Var(_: typedesc[Type], env: TypeEnv): Type =
    env.newVar()
