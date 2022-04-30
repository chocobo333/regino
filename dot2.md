```dot
digraph order {
  node [
    shape = none
  ];
  edge [
    dir = back
  ];
  "'b(Bottom, Unit)"
  "float"
  "int"
  "'b(Bottom, Unit)" -> "int"
  "float" -> "'b(Bottom, Unit)"
}
```