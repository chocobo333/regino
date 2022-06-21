
import decls

proc isOdd*(a: int, n: bool = false): bool {.exportc: "odd".} =
    if a == 0:
        false
    else:
        isEven(a-1)
