
import ../il
import ../typeenv

proc infer*(self: Expression, env: TypeEnv, global: bool = false): Value {.importc: "infer_e".}
proc infer*(self: Statement, env: TypeEnv, global: bool = false): Value {.importc: "infer_s".}

proc eval*(self: Expression, env: TypeEnv, global: bool = false): Value {.importc: "eval_e".}
proc eval*(self: TypeExpression, env: TypeEnv, ident: Ident, global: bool = false): Value {.importc: "eval_te".}

proc check*(self: Expression, env: TypeEnv) {.importc: "check_e", nodecl.}
