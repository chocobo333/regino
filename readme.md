# Regino
### Regino is a programming language **aimed** to following features:
* Region based memory management.
  * No gc. No manual memory management.
  * and, No region annotation.
* Have simple type system
* Have Compile-time evaluation and generation of code
  * macro or template
* Have and Provide ffi(foreign function interface)
* Provide lsp(Language server protocol)
* Compile to [llvm ir](https://llvm.org/docs/LangRef.html) or [wasm](https://developer.mozilla.org/en-US/docs/WebAssembly)

# Try it
Currently, this project is implemented in the [Nim language](https://nim-lang.org/) and llvm.


```sh
nimble install -y
regino compile -f:test/test05.rgn
lli test/test.ll # outputs fib(10)
```