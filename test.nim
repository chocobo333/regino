

type
    A = ref object
        a: int
    B = ref A

var
    a: A = new A
    b = a

a = A(a: 3)
echo a[]
echo b[]

b = A(a: 4)
echo a[]
echo b[]
