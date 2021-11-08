
import relations
import strformat


type
    Order*[T] = object
        primal*: Relation[T, T] # for <
        dual*: Relation[T, T]   # for >

proc newOrder*[T](): Order[T] =
    Order[T](
        primal: newRelation[T, T](),
        dual: newRelation[T, T]()
    )

proc contains*[T](self: Order[T], val: (T, T)): bool =
    self.primal.contains val
proc add*[T](self: var Order[T], val: (T, T)) =
    self.primal[val[0]] = val[1]
    self.dual[val[1]] = val[0]
proc `$`*[T](self: Order[T]): string =
    if self.primal.len == 0:
        result = "{}"
    else:
        result = "{"
        for key, val in pairs(self.primal):
            for val in val.items:
                result.add fmt"{key} < {val}, "
        result = result[0..^3]
        result.add("}")
