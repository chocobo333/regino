```dot
digraph order {
  node [
    shape = none
  ];
  edge [
    dir = back
  ];
  "(int) -> 'p(Bottom, Unit)"
  "'o(int, int)"
  "int"
  "(int) -> int"
  "(Unit) -> 'p(Bottom, Unit)"
  "int" -> "'o(int, int)"
  "(int) -> 'p(Bottom, Unit)" -> "(int) -> int"
  "(int) -> int" -> "(Unit) -> 'p(Bottom, Unit)"
}
```