
proc isEven*(a: int, n: int = 0): bool {.importc: "even", nodecl.}
proc isOdd*(a: int, n: bool = false): bool {.importc: "odd", nodecl.}
