

type
    A = object
        b: B
    B = object
        i: int

var
    a = A(b: B(i: 3))
    b = 3

echo a

(a.b.i, b) = (4, 5)
echo a
echo b
