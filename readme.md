# Regino
### Regino is a programming language aimed to following features:
* Region based memory management.
  * No gc. No manual memory management.
  * and, No region annotation.
* Have simple type system
* Have Compile-time evaluation and generation of code
  * macro or template
* Have and Provide ffi(foreign function interface)
* Provide lsp(Language server protocol)

# Try it
Currently, this project is implemented in the [Nim language](https://nim-lang.org/) and llvm.

```sh
nim c -r src/compiler.nim
lli test/test.ll
```