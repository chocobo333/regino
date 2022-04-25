
import sets
import tables


type
    Relation*[A, B] = Table[A, HashSet[B]]


proc newRelation*[A, B](): Relation[A, B] = initTable[A, HashSet[B]]()
proc contains*[A, B](self: Relation[A, B], val: (A, B)): bool =
    let (a, b) = val
    if a in self:
        b in self[a]
    else:
        false

proc `[]=`*[A, B](self: var Relation[A, B], a: A, b: B) =
    if a in self:
        self[a].incl b
    else:
        self[a] = initHashSet[B]()
        self[a].incl b

proc incl*[A, B](self: Relation[A, B], val: (A, B)) =
    let (a, b) = val
    self[a] = b
proc remove*[A, B](self: var Relation[A, B], val: (A, B)) =
    let (a, b) = val
    if a in self:
        self[a].excl(b)
        if self[a].len == 0:
            self.del(a)

proc `$`*[A, B](self: Relation[A, B]): string =
    if self.len == 0:
        result = "{:}"
    else:
        result = "{"
        for key, val in pairs(self):
            if result.len > 1: result.add(", ")
            result.addQuoted(key)
            result.add(": ")
            result.addQuoted(val)
        result.add("}")
