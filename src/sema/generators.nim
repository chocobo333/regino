
type
    Generator*[T: Ordinal] = object
        current: T

proc get*[T](self: var Generator[T]): T =
    result = self.current
    inc self.current
