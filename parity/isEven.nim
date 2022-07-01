
import decls

proc isEven*(a: int, n: int = 0): bool {.exportc: "even".} =
    if a == 0:
        true
    else:
        isOdd(a-1)

proc isOdd2*(a: int): bool =
    if a == 0:
        false
    else:
        isEven(a-1)
proc isOdd3*(a: int): bool =
    isOdd(a)
