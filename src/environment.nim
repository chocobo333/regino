
type
    SymId* = distinct uint
    TypeVarId* = distinct uint
    Environment* = ref object
        symid: SymId
        typevarid: TypeVarId


proc `$`*(self: SymId): string {.borrow.}
proc `$`*(self: TypeVarId): string {.borrow.}

proc newEnvironment*(): Environment =
    Environment()

proc newSymId*(self: Environment): SymId =
    inc self.symid
    self.symid

proc newTypeVarId*(self: Environment): TypeVarId =
    inc self.typevarid
    self.typevarid