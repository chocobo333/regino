
import options
import sequtils
import sugar
import sets
import tables
import algorithm

import inst
import ../il
import ../typeenv
import ../errors

import ../utils
import ../lineinfos

import coerce
import resolve

proc infer*(self: Expression, env: TypeEnv, global: bool = false): Value {.importc: "infer_e".}
proc infer*(self: Statement, env: TypeEnv, global: bool = false): Value {.importc: "infer_s".}

proc eval*(self: Expression, env: TypeEnv, global: bool = false): Value {.importc: "eval_e".}
proc eval*(self: TypeExpression, env: TypeEnv, ident: Ident, global: bool = false): Value {.importc: "eval_te".}

proc check*(self: Expression, env: TypeEnv) {.importc: "check_e".}
