
import options

import parsers


type
    Emptiable* = concept x, type T
        T.empty is T
    From*[T] = T | FromEx[T]
    FromEx*[U] = concept x, type T
        var u: U
        u.to(type T)
    To*[T] = T | ToEx[T]
    ToEx*[U] = concept x, type T
        when U is string:
            $x is string
        else:
            x.to(type U)
    Takenable* = concept x
        type T = typeof(x)
        x.taken(string) is Option[(T, T)]
        x.startsWith(string) is bool
    Matchable* = concept x
        type T = typeof(x)
        x.takeMatch(string) is Result[T, T, string]