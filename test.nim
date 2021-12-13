

type
    A = object
        b: B
    B = object
        i: int
    C[T] = T

var
    a = A(b: B(i: 3))
    b = 3

echo a

(a.b.i, b) = (4, 5)
echo a
echo b

let c: C[int] = when true:
    3
else:
    true
