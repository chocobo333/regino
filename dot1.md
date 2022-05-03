```dot
digraph order {
  node [
    shape = none
  ];
  edge [
    dir = back
  ];
  "(float) -> 'e(Bottom, ())"
  "'f((float) -> int or (float) -> float, (float) -> int or (float) -> float)"
  "'e(Bottom, ())"
  "'d(Bottom, ())"
  "'i((float) -> int or (float) -> float, (float) -> int or (float) -> float)"
  "'h(Bottom, ())"
  "'g(Bottom, ())"
  "'l((float) -> int or (float) -> float, (float) -> int or (float) -> float)"
  "'k(Bottom, ())"
  "'j(Bottom, ())"
  "float"
  "int"
  "(float) -> 'h(Bottom, ())"
  "(float) -> 'k(Bottom, ())"
  "(float) -> 'k(Bottom, ())" -> "'l((float) -> int or (float) -> float, (float) -> int or (float) -> float)"
  "float" -> "'g(Bottom, ())"
  "'d(Bottom, ())" -> "'e(Bottom, ())"
  "'d(Bottom, ())" -> "int"
  "int" -> "'d(Bottom, ())"
  "'g(Bottom, ())" -> "'h(Bottom, ())"
  "'g(Bottom, ())" -> "float"
  "'j(Bottom, ())" -> "'k(Bottom, ())"
  "(float) -> 'e(Bottom, ())" -> "'f((float) -> int or (float) -> float, (float) -> int or (float) -> float)"
  "(float) -> 'h(Bottom, ())" -> "'i((float) -> int or (float) -> float, (float) -> int or (float) -> float)"
}
```