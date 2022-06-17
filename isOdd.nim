
import decls

proc isOdd*(a: int): bool {.exportc.} =
    if a == 0:
        false
    else:
        isEven(a-1)
