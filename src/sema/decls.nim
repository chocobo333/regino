
import ../il
import ../typeenv
import ../projects/projects

proc infer*(self: Expression, env: TypeEnv, project: Project, global: bool = false): Value {.importc: "infer_e".}
proc infer*(self: Statement, env: TypeEnv, project: Project, global: bool = false): Value {.importc: "infer_s".}

proc eval*(self: Expression, env: TypeEnv, project: Project, global: bool = false): Value {.importc: "eval_e".}
proc eval*(self: TypeExpression, env: TypeEnv, project: Project, ident: Ident, global: bool = false): Value {.importc: "eval_te".}

proc check*(self: Expression, env: TypeEnv, project: Project) {.importc: "check_e", nodecl.}
