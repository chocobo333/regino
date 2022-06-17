
import decls

proc isEven*(a: int): bool {.exportc.} =
    if a == 0:
        true
    else:
        isOdd(a-1)
